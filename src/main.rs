use backer::{nodes::*, Layout, Node, NodeWith};
use rand::{rngs::StdRng, Rng, SeedableRng};
use std::{fmt::Display, ops::RangeInclusive};

use crossterm::event::{self, Event, KeyCode, KeyEvent};
use ratatui::{
    layout::Rect,
    style::Stylize,
    text::Text,
    widgets::{Block, BorderType},
    Frame,
};

fn main() {
    let mut app = App {
        rng: StdRng::seed_from_u64(0),
        day: 0,
        character: None,
        current_dungeon: None,
        log: RollLog { rolls: vec![] },
        battle_state: None,
        battle_status: "".to_owned(),
        resting: false,
        inventory_open: false,
        market_open: false,
        market: Market {
            items: vec![],
            refresh_cost: 1,
        },
        room_looted: false,
        inventory_selection: 0,
        market_selection: 0,
    };
    app.run();
}

struct App<Rng: RngSource> {
    rng: Rng,
    character: Option<Character>,
    day: usize,
    current_dungeon: Option<Dungeon>,
    battle_state: Option<Battle>,
    resting: bool,
    battle_status: String,
    inventory_open: bool,
    market_open: bool,
    market: Market,
    market_selection: usize,
    room_looted: bool,
    inventory_selection: usize,
    log: RollLog,
}

impl<Rng: RngSource> App<Rng> {
    fn run(&mut self) {
        let mut terminal = ratatui::init();
        loop {
            terminal
                .draw(|frame| self.draw(frame))
                .expect("failed to draw frame");
            match event::read().expect("Event read failure") {
                Event::Key(KeyEvent {
                    code: KeyCode::Enter,
                    ..
                }) => {
                    if self.inventory_open || self.market_open {
                        continue;
                    }
                    match (&mut self.current_dungeon, &mut self.character) {
                        (None, Some(character)) => {
                            self.day += 1;
                            self.current_dungeon = Some(Dungeon::generate(
                                character.level,
                                &mut self.rng,
                                &mut self.log,
                            ));
                        }
                        (Some(dungeon), Some(character)) => {
                            if self.resting {
                                self.resting = false;
                                dungeon.current_room += 1;
                                self.battle_state = None;
                                self.market.refresh(character, &mut self.rng, true);
                            } else if self.battle_state == Some(Battle::NPCDead) && !self.resting {
                                self.battle_status = character.rest(&mut self.rng, &mut self.log)
                                    + "\n'Enter' the next room";
                                self.resting = true
                            }
                        }
                        (None, None) => {
                            self.character = Some(Character::create_player(
                                0,
                                0,
                                false,
                                &mut self.rng,
                                &mut self.log,
                            ));
                            if let Some(ref mut character) = self.character {
                                self.market.refresh(character, &mut self.rng, true);
                            }
                        }
                        (Some(_), None) => (),
                    }
                }
                Event::Key(KeyEvent {
                    code: KeyCode::Char('k'),
                    ..
                }) => {
                    if self.inventory_open || self.market_open || self.resting {
                        continue;
                    }
                    if let (Some(dungeon), Some(character)) =
                        (&mut self.current_dungeon, &mut self.character)
                    {
                        if self.battle_state.is_none() {
                            self.room_looted = false
                        }
                        let result = Character::battle(
                            self.battle_state.unwrap_or(Battle::Initiative),
                            character,
                            &mut dungeon.rooms[dungeon.current_room].enemy,
                            Difficulty::Easy,
                            &mut self.rng,
                            &mut self.log,
                        );
                        self.battle_state = Some(result.0);
                        self.battle_status = result.1;
                        if result.0 == Battle::NPCDead && !self.room_looted {
                            character.xp += 1000;
                            character
                                .inventory
                                .push(dungeon.rooms[dungeon.current_room].loot);
                            self.room_looted = true
                        }
                    }
                }
                Event::Key(KeyEvent {
                    code: KeyCode::Char('i'),
                    ..
                }) => {
                    self.inventory_open = !self.inventory_open;
                }
                Event::Key(KeyEvent {
                    code: KeyCode::Char('m'),
                    ..
                }) => {
                    self.market_open = !self.market_open;
                }
                Event::Key(KeyEvent {
                    code: KeyCode::Down,
                    ..
                }) => {
                    if let Some(ref mut character) = self.character {
                        if self.inventory_open
                            && self.inventory_selection + 1 < character.inventory.len()
                        {
                            self.inventory_selection += 1;
                        }
                        if self.market_open && self.market_selection + 1 < self.market.items.len() {
                            self.market_selection += 1;
                        }
                    }
                }
                Event::Key(KeyEvent {
                    code: KeyCode::Up, ..
                }) => {
                    if self.inventory_open && self.inventory_selection >= 1 {
                        self.inventory_selection -= 1;
                    }
                    if self.market_open && self.market_selection >= 1 {
                        self.market_selection -= 1;
                    }
                }
                Event::Key(KeyEvent {
                    code: KeyCode::Char('e'),
                    ..
                }) => {
                    if let Some(ref mut character) = self.character {
                        if self.inventory_open {
                            let unequipped = character.equipped;
                            character.equipped =
                                Some(character.inventory[self.inventory_selection]);
                            character.inventory.remove(self.inventory_selection);
                            if let Some(unequipped) = unequipped {
                                character.inventory.push(unequipped)
                            }
                        }
                    }
                }
                Event::Key(KeyEvent {
                    code: KeyCode::Char('s'),
                    ..
                }) => {
                    if let Some(ref mut character) = self.character {
                        if self.inventory_open && !character.inventory.is_empty() {
                            character.gold += character.inventory[self.inventory_selection].value();
                            character.inventory.remove(self.inventory_selection);
                            if self.inventory_selection >= 1 {
                                self.inventory_selection -= 1
                            }
                        }
                    }
                }
                Event::Key(KeyEvent {
                    code: KeyCode::Char('r'),
                    ..
                }) => {
                    if let Some(ref mut character) = self.character {
                        if self.market_open {
                            self.market.refresh(character, &mut self.rng, false);
                        }
                    }
                }
                Event::Key(KeyEvent {
                    code: KeyCode::Char('b'),
                    ..
                }) => {
                    if let Some(ref mut character) = self.character {
                        if self.market_open && !self.market.items.is_empty() {
                            let bought = self.market.items[self.market_selection];
                            let cost = self.market.items[self.market_selection].value();
                            if cost <= character.gold {
                                character.gold -= self.market.items[self.market_selection].value();
                                character.inventory.push(bought);
                                self.market.items.remove(self.market_selection);
                                if self.market_selection >= 1 {
                                    self.market_selection -= 1
                                }
                            }
                        }
                    }
                }
                Event::Key(KeyEvent {
                    code: KeyCode::Char('x'),
                    ..
                }) => break,
                _ => (),
            }
        }
        ratatui::restore();
    }
    fn draw(&mut self, frame: &mut Frame<'_>) {
        let inventory_open = self.inventory_open;
        let market_open = self.market_open;
        Layout::new_with(move |i, a| {
            if inventory_open {
                inventory(i, a)
            } else if market_open {
                market(i, a)
            } else {
                home(i, a)
            }
        })
        .draw_with(area(frame.area()), frame, self);
    }
}

fn home<'a, 'frame, Rng: RngSource>(
    frame: &'a mut Frame<'frame>,
    app: &mut App<Rng>,
) -> NodeWith<Frame<'frame>, App<Rng>> {
    column(vec![
        row(vec![text("e(x)it   (i)nventory   (m)arket")]).height(2.),
        row(vec![
            column(vec![
                text_centered(format!("Day: {}", app.day)).height(2.),
                if let Some(dungeon) = &app.current_dungeon {
                    group(vec![
                        text_centered_blink(format!("{}", dungeon)).height(2.),
                        stack(vec![
                            block("Room"),
                            column(vec![
                                text_centered(format!(
                                    "Enemy: lvl {} {:?} {}HP",
                                    dungeon.rooms[dungeon.current_room].enemy.level,
                                    dungeon.rooms[dungeon.current_room].enemy.background,
                                    dungeon.rooms[dungeon.current_room].enemy.hp,
                                )),
                                text_centered(format!(
                                    "Loot: {}",
                                    dungeon.rooms[dungeon.current_room].loot
                                )),
                                if let Some(_bstate) = app.battle_state {
                                    text_centered(app.battle_status.clone())
                                } else {
                                    text_centered("'k' to battle")
                                },
                            ])
                            .pad(1.),
                        ]),
                    ])
                } else if app.character.is_some() {
                    text_centered_blink("'Enter' the dungeon")
                } else {
                    empty()
                },
            ]),
            stack(vec![
                block("Rolls"),
                column(
                    app.log
                        .rolls
                        .iter()
                        .map(|r| text(format!("{}", r.clone())).height(1.))
                        .collect(),
                )
                .pad(1.),
            ])
            .width(25.),
        ]),
        character_panel(frame, app),
    ])
}

fn market<'a, 'frame, Rng: RngSource>(
    frame: &'a mut Frame<'frame>,
    app: &mut App<Rng>,
) -> NodeWith<Frame<'frame>, App<Rng>> {
    column(vec![
        stack(vec![
            block(format!(
                "(m)arket          (↕) to select{}",
                if app.character.as_ref().map(|c| c.gold).unwrap_or(0) >= app.market.refresh_cost {
                    format!(
                        "          (r) buy restock: {} gold",
                        app.market.refresh_cost
                    )
                } else {
                    "".to_string()
                }
            )),
            column(vec![group(
                app.market
                    .items
                    .iter()
                    .enumerate()
                    .map(|(i, item)| {
                        text(format!(
                            "{} {} {}",
                            if i == app.market_selection { "-->" } else { "" },
                            item.clone(),
                            if i == app.market_selection {
                                format!("--> (b)uy for {} gold", item.value())
                            } else {
                                "".to_string()
                            },
                        ))
                        .height(1.)
                    })
                    .collect(),
            )]),
        ]),
        character_panel(frame, app),
    ])
}

fn inventory<'a, 'frame, Rng: RngSource>(
    frame: &'a mut Frame<'frame>,
    app: &mut App<Rng>,
) -> NodeWith<Frame<'frame>, App<Rng>> {
    column(vec![
        stack(vec![
            block("(i)nventory          (↕) to select"),
            column(vec![group(
                app.character
                    .as_ref()
                    .map(|c| c.inventory.clone())
                    .unwrap_or_default()
                    .iter()
                    .enumerate()
                    .map(|(i, item)| {
                        text(format!(
                            "{} {} {}",
                            if i == app.inventory_selection {
                                "-->"
                            } else {
                                ""
                            },
                            item.clone(),
                            if i == app.inventory_selection {
                                format!("--> (e)quip (s)ell for {} gold", item.value())
                            } else {
                                "".to_string()
                            },
                        ))
                        .height(1.)
                    })
                    .collect(),
            )]),
        ]),
        character_panel(frame, app),
    ])
}

fn character_panel<'a, 'frame, Rng: RngSource>(
    frame: &'a mut Frame<'frame>,
    app: &mut App<Rng>,
) -> NodeWith<Frame<'frame>, App<Rng>> {
    stack(vec![if let Some(character) = &app.character {
        group(vec![
            block(format!(
                "{:?} {}: lvl: {}",
                character.background,
                Effect::from(character.background),
                character.level
            )),
            row(vec![
                column(vec![
                    text("Base:"),
                    text(format!("STR: {}", character.strength)),
                    text(format!("DEX: {}", character.dexterity)),
                    text(format!("INT: {}", character.intelligence)),
                    text(format!("CON: {}", character.constitution)),
                ])
                .width(8.),
                column(vec![
                    text("Total:"),
                    text(format!("STR: {}", character.effective_strength())),
                    text(format!("DEX: {}", character.effective_dexterity())),
                    text(format!("INT: {}", character.effective_intelligence())),
                    text(format!("CON: {}", character.effective_constitution())),
                ])
                .width(8.),
                column(vec![
                    if let Some(item) = character.equipped {
                        text(format!("Equipped: {}", item))
                    } else {
                        empty()
                    },
                    text(format!("Gold: {}", character.gold)),
                    text(format!("HP: {}", character.hp)),
                    text(format!(
                        "{} {}",
                        character.primary_trait,
                        Effect::from(character.primary_trait)
                    )),
                    text(format!(
                        "{} {}",
                        character.secondary_trait,
                        Effect::from(character.secondary_trait)
                    )),
                ]),
                column(vec![text(format!("XP: {}", character.xp))]),
            ])
            .pad(1.),
        ])
    } else {
        block("Press 'Enter' to roll character")
    }])
    .height(7.)
}

fn text<'frame, Rng: RngSource>(
    text: impl AsRef<str> + 'static,
) -> NodeWith<Frame<'frame>, App<Rng>> {
    draw_with(move |area, frame: &mut Frame<'_>, _| {
        frame.render_widget(Text::raw(text.as_ref()), rect(area))
    })
}

fn text_centered<'frame, Rng: RngSource>(
    text: impl AsRef<str> + 'static,
) -> NodeWith<Frame<'frame>, App<Rng>> {
    draw_with(move |area, frame: &mut Frame<'_>, _| {
        frame.render_widget(Text::raw(text.as_ref()).centered(), rect(area))
    })
}

fn text_centered_blink<'frame, Rng: RngSource>(
    text: impl AsRef<str> + 'static,
) -> NodeWith<Frame<'frame>, App<Rng>> {
    draw_with(move |area, frame: &mut Frame<'_>, _| {
        frame.render_widget(Text::raw(text.as_ref()).centered().slow_blink(), rect(area))
    })
}

fn block<'frame, Rng: RngSource>(
    text: impl AsRef<str> + 'static,
) -> NodeWith<Frame<'frame>, App<Rng>> {
    draw_with(move |area, frame: &mut Frame<'_>, _| {
        frame.render_widget(
            Block::bordered()
                .border_type(BorderType::Rounded)
                .title_alignment(ratatui::layout::Alignment::Center)
                .title(text.as_ref()),
            rect(area),
        )
    })
}

fn rect(area: backer::models::Area) -> Rect {
    Rect {
        x: area.x as u16,
        y: area.y as u16,
        width: area.width as u16,
        height: area.height as u16,
    }
}
fn area(rect: Rect) -> backer::models::Area {
    backer::models::Area::new(
        rect.x as f32,
        rect.y as f32,
        rect.width as f32,
        rect.height as f32,
    )
}

struct Game {
    pc: Character,
    dungeon: Dungeon,
}

#[derive(Debug, Clone)]
struct Dungeon {
    current_room: usize,
    rooms: Vec<Room>,
}

#[derive(Debug, Clone)]
struct Market {
    items: Vec<Item>,
    refresh_cost: usize,
}

impl Market {
    fn refresh(&mut self, pc: &mut Character, rng: &mut impl RngSource, free: bool) {
        if self.refresh_cost <= pc.gold {
            self.items = (0..4).map(|_| Item::random(rng)).collect();
            if !free {
                pc.gold -= self.refresh_cost;
                self.refresh_cost += (self.refresh_cost as f32 * 0.2).max(1.) as usize
            }
        }
    }
}

impl Display for Dungeon {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for i in 0..self.rooms.len() {
            match i.cmp(&self.current_room) {
                std::cmp::Ordering::Less => write!(f, "x")?,
                std::cmp::Ordering::Equal => write!(f, "!")?,
                std::cmp::Ordering::Greater => write!(f, "?")?,
            }
            write!(f, "-")?
        }
        write!(f, ">")
    }
}

impl Dungeon {
    fn generate(level: usize, rng: &mut impl RngSource, log: &mut RollLog) -> Self {
        Self {
            current_room: 0,
            rooms: (0..5)
                .map(|i| Room {
                    enemy: Character::create_player(
                        level, level,
                        // i + Roll::D2.result(rng, log, "Enemy Strength"),
                        true, rng, log,
                    ),
                    loot: Item::random(rng),
                })
                .collect(),
        }
    }
}

#[derive(Debug, Clone)]
struct Room {
    enemy: Character,
    loot: Item,
}

#[derive(Debug, Clone)]
struct Character {
    level: usize,
    xp: usize,
    strength: usize,
    dexterity: usize,
    intelligence: usize,
    constitution: usize,
    hp: usize,
    gold: usize,
    power: Option<Power>,
    primary_trait: CharacterTrait,
    secondary_trait: CharacterTrait,
    background: CharacterBackground,
    equipped: Option<Item>,
    inventory: Vec<Item>,
}

pub trait RngSource {
    fn gen_u32(&mut self, range: RangeInclusive<usize>) -> usize;
}

impl<R: Rng> RngSource for R {
    fn gen_u32(&mut self, range: RangeInclusive<usize>) -> usize {
        self.gen_range(range)
    }
}

impl Character {
    fn create_player(
        level: usize,
        modifier: usize,
        enemy: bool,
        rng: &mut impl RngSource,
        log: &mut RollLog,
    ) -> Self {
        let primary_trait = CharacterTrait::from(Roll::D12.result_mute(rng, log, enemy, "Trait 1"));
        let mut secondary_trait =
            CharacterTrait::from(Roll::D12.result_mute(rng, log, enemy, "Trait 2"));
        while primary_trait.contradicts(&primary_trait) {
            secondary_trait =
                CharacterTrait::from(Roll::D12.result_mute(rng, log, enemy, "Trait Reroll"));
        }
        let background =
            CharacterBackground::from(Roll::D20.result_mute(rng, log, enemy, "Background"));
        let con = Roll::D4.result_mute(rng, log, enemy, "Constitution");
        Self {
            level: level + modifier,
            xp: level + modifier,
            strength: Roll::D4.result_mute(rng, log, enemy, "Strength"),
            dexterity: Roll::D4.result_mute(rng, log, enemy, "Dexterity"),
            intelligence: Roll::D4.result_mute(rng, log, enemy, "Intelligence"),
            constitution: con,
            hp: (0..(level + con))
                .map(|_| Roll::D8.result_mute(rng, log, enemy, "HP"))
                .sum(),
            gold: (0..10)
                .map(|_| Roll::D10.result_mute(rng, log, enemy, "Gold"))
                .sum(),
            power: if Roll::D4.result_mute(rng, log, enemy, "Bestow Power") == 4 {
                Some(Power::from(Roll::D12.result_mute(rng, log, enemy, "Power")))
            } else {
                None
            },
            primary_trait,
            secondary_trait,
            background,
            equipped: background.default_equipped_item(),
            inventory: vec![],
        }
    }
    fn rest(&mut self, rng: &mut impl RngSource, log: &mut RollLog) -> String {
        if let Some((i, _)) = self
            .inventory
            .iter()
            .enumerate()
            .find(|(_, item)| matches!(item, Item::Utility(Utility::Ration)))
        {
            let result = Roll::D6.result(rng, log, "Rest");
            self.inventory.remove(i);
            self.hp += result;
            format!("Consumed a ration, rested for {} HP", result)
        } else {
            let result = Roll::D2.result(rng, log, "Rest");
            format!("No rations left, rested for {} HP", result)
        }
    }

    fn battle(
        state: Battle,
        pc: &mut Character,
        npc: &mut Character,
        difficulty: Difficulty,
        rng: &mut impl RngSource,
        log: &mut RollLog,
    ) -> (Battle, String) {
        if pc.hp == 0 {
            return (Battle::PCDead, "You died".to_owned());
        }
        if npc.hp == 0 {
            pc.inventory.append(&mut npc.inventory);
            return (Battle::NPCDead, "You won! 'enter' to rest".to_owned());
        }
        match state {
            Battle::Initiative => (
                Battle::InitiativeResult(Roll::D6.result(rng, log, "Initiative") > 3),
                "You roll initiative".to_owned(),
            ),
            Battle::InitiativeResult(pc_starts) => {
                if pc_starts {
                    (Battle::PCTurn, "Success! You move first".to_owned())
                } else {
                    (Battle::NPCTurn, "Fail! Enemy moves first".to_owned())
                }
            }
            Battle::NPCTurn => (
                Battle::PCTurn,
                Self::combat(pc, npc, rng, difficulty, true, log),
            ),
            Battle::PCTurn => (
                Battle::PCTurn,
                Self::combat(pc, npc, rng, difficulty, false, log),
            ),
            Battle::NPCDead | Battle::PCDead => (state, "".to_owned()),
        }
    }
    fn combat(
        pc: &mut Self,
        npc: &mut Self,
        rng: &mut impl RngSource,
        difficulty: Difficulty,
        npc_turn: bool,
        log: &mut RollLog,
    ) -> String {
        fn success(
            difficulty: Difficulty,
            stat: usize,
            advantage: bool,
            for_npc: bool,
            rng: &mut impl RngSource,
            log: &mut RollLog,
        ) -> bool {
            let roll = if advantage {
                Roll::D20
                    .result(rng, log, "Combat 1")
                    .max(Roll::D20.result(rng, log, "Combat 2"))
            } else {
                Roll::D20
                    .result(rng, log, "Combat 1")
                    .min(Roll::D20.result(rng, log, "Combat 2"))
            };
            let resolve = roll + stat;
            if for_npc {
                match difficulty {
                    Difficulty::Easy => resolve < 8,
                    Difficulty::Normal => resolve < 12,
                    Difficulty::Hard => resolve < 16,
                }
            } else {
                match difficulty {
                    Difficulty::Easy => resolve >= 8,
                    Difficulty::Normal => resolve >= 12,
                    Difficulty::Hard => resolve >= 16,
                }
            }
        }
        fn npc_attack_pc(
            pc: &mut Character,
            npc: &mut Character,
            defend_successful: bool,
            rng: &mut impl RngSource,
            log: &mut RollLog,
        ) -> String {
            let damage = if let Some(Item::Weapon(weapon)) = npc.equipped {
                weapon.attack(rng, log)
            } else {
                Roll::D2.result(rng, log, "Unarmed melee")
            };
            let defense = if defend_successful {
                if let Some(Item::Armor(armor)) = pc.equipped {
                    armor.defend(rng, log)
                } else {
                    0
                }
            } else {
                0
            };

            let result: usize;
            if damage <= npc.hp {
                if defense <= damage {
                    result = damage - defense;
                    pc.hp -= damage - defense;
                } else {
                    result = 0;
                    pc.hp -= 0;
                }
            } else {
                result = npc.hp;
                pc.hp = 0;
            }

            format!("\nHit the enemy for {}", result)
        }
        fn pc_attack_npc(
            pc: &mut Character,
            npc: &mut Character,
            defend_successful: bool,
            rng: &mut impl RngSource,
            log: &mut RollLog,
        ) -> String {
            let damage = if let Some(Item::Weapon(weapon)) = pc.equipped {
                weapon.attack(rng, log)
            } else {
                Roll::D2.result(rng, log, "Unarmed melee")
            };
            let defense = if defend_successful {
                if let Some(Item::Armor(armor)) = npc.equipped {
                    armor.defend(rng, log)
                } else {
                    0
                }
            } else {
                0
            };

            let result: usize;
            if damage <= npc.hp {
                if defense <= damage {
                    result = damage - defense;
                    npc.hp -= damage - defense;
                } else {
                    result = 0;
                    npc.hp -= 0;
                }
            } else {
                result = npc.hp;
                npc.hp = 0;
            }

            format!("\nHit the enemy for {}", result)
        }
        let npc_advantage = npc.xp > pc.xp;
        if npc_turn {
            let relevant_stat = if let Some(Item::Weapon(weapon)) = npc.equipped {
                if weapon.ranged_weapon() {
                    npc.dexterity
                } else {
                    npc.strength
                }
            } else {
                npc.strength
            };
            let attack_successful =
                success(difficulty, relevant_stat, npc_advantage, true, rng, log);

            if attack_successful {
                let defend_successful =
                    success(difficulty, pc.constitution, !npc_advantage, false, rng, log);
                "Enemy attacks!".to_owned()
                    + npc_attack_pc(pc, npc, defend_successful, rng, log).as_str()
            } else {
                let defend_successful =
                    success(difficulty, npc.constitution, npc_advantage, true, rng, log);
                "Enemy misses!".to_owned()
                    + pc_attack_npc(pc, npc, defend_successful, rng, log).as_str()
            }
        } else {
            let relevant_stat = if let Some(Item::Weapon(weapon)) = pc.equipped {
                if weapon.ranged_weapon() {
                    pc.dexterity
                } else {
                    pc.strength
                }
            } else {
                pc.strength
            };
            let attack_successful =
                success(difficulty, relevant_stat, !npc_advantage, false, rng, log);
            if attack_successful {
                let defend_successful =
                    success(difficulty, npc.constitution, npc_advantage, true, rng, log);
                "You attack!".to_owned()
                    + pc_attack_npc(pc, npc, defend_successful, rng, log).as_str()
            } else {
                let defend_successful =
                    success(difficulty, pc.constitution, !npc_advantage, false, rng, log);
                "You miss!".to_owned()
                    + npc_attack_pc(pc, npc, defend_successful, rng, log).as_str()
            }
        }
    }
    fn effective_strength(&self) -> usize {
        (self.strength as i32
            + Effect::from(self.background).str.unwrap_or(0)
            + Effect::from(self.primary_trait).str.unwrap_or(0)
            + Effect::from(self.secondary_trait).str.unwrap_or(0)) as usize
    }
    fn effective_dexterity(&self) -> usize {
        (self.dexterity as i32
            + Effect::from(self.background).dex.unwrap_or(0)
            + Effect::from(self.primary_trait).dex.unwrap_or(0)
            + Effect::from(self.secondary_trait).dex.unwrap_or(0)) as usize
    }
    fn effective_intelligence(&self) -> usize {
        (self.intelligence as i32
            + Effect::from(self.background).int.unwrap_or(0)
            + Effect::from(self.primary_trait).int.unwrap_or(0)
            + Effect::from(self.secondary_trait).int.unwrap_or(0)) as usize
    }
    fn effective_constitution(&self) -> usize {
        (self.constitution as i32
            + Effect::from(self.background).con.unwrap_or(0)
            + Effect::from(self.primary_trait).con.unwrap_or(0)
            + Effect::from(self.secondary_trait).con.unwrap_or(0)) as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Battle {
    Initiative,
    InitiativeResult(bool),
    NPCTurn,
    PCTurn,
    NPCDead,
    PCDead,
}

#[derive(Debug, Clone, Copy)]
enum Difficulty {
    Easy,
    Normal,
    Hard,
}

#[derive(Debug, Clone, Copy)]
enum Roll {
    D2,
    D4,
    D6,
    D8,
    D10,
    D12,
    D20,
}

impl Display for Roll {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Roll::D2 => write!(f, "D2"),
            Roll::D4 => write!(f, "D4"),
            Roll::D6 => write!(f, "D6"),
            Roll::D8 => write!(f, "D8"),
            Roll::D10 => write!(f, "D10"),
            Roll::D12 => write!(f, "D12"),
            Roll::D20 => write!(f, "D20"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum CharacterBackground {
    Slave,
    Homeless,
    Farmer,
    Merchant,
    Musician,
    Medic,
    Sailor,
    Wanderer,
    Beekeeper,
    Hunter,
    Brute,
    Assassin,
    Cultist,
    Politician,
    Blacksmith,
    Astrologist,
    Tanner,
    Scholar,
    Mercenary,
    Knight,
}
impl CharacterBackground {
    fn money(&self) -> usize {
        match self {
            CharacterBackground::Merchant => 100,
            CharacterBackground::Politician => 200,
            _ => 0,
        }
    }
}
impl From<CharacterTrait> for Effect {
    fn from(value: CharacterTrait) -> Self {
        match value {
            CharacterTrait::WeakMinded => Effect {
                hp: None,
                str: None,
                dex: None,
                int: Some(-1),
                con: None,
                init: None,
            },
            CharacterTrait::Depressive => Effect {
                hp: None,
                str: None,
                dex: None,
                int: None,
                con: Some(-1),
                init: None,
            },
            CharacterTrait::Frail => Effect {
                hp: None,
                str: Some(-1),
                dex: None,
                int: None,
                con: None,
                init: None,
            },
            CharacterTrait::Clumsy => Effect {
                hp: None,
                str: None,
                dex: Some(-1),
                int: None,
                con: None,
                init: None,
            },
            CharacterTrait::BadStomach => Effect {
                hp: Some(-2),
                str: None,
                dex: None,
                int: None,
                con: None,
                init: None,
            },
            CharacterTrait::Alcoholic => Effect {
                hp: None,
                str: None,
                dex: None,
                int: None,
                con: None,
                init: Some(-1),
            },
            CharacterTrait::EagleEye => Effect {
                hp: None,
                str: None,
                dex: None,
                int: None,
                con: None,
                init: Some(1),
            },
            CharacterTrait::Healer => Effect {
                hp: Some(2),
                str: None,
                dex: None,
                int: None,
                con: None,
                init: None,
            },
            CharacterTrait::Wise => Effect {
                hp: None,
                str: None,
                dex: None,
                int: Some(1),
                con: None,
                init: None,
            },
            CharacterTrait::Survivalist => Effect {
                hp: None,
                str: None,
                dex: None,
                int: None,
                con: Some(1),
                init: None,
            },
            CharacterTrait::Brawler => Effect {
                hp: None,
                str: Some(1),
                dex: None,
                int: None,
                con: None,
                init: None,
            },
            CharacterTrait::QuickReflex => Effect {
                hp: None,
                str: None,
                dex: Some(1),
                int: None,
                con: None,
                init: None,
            },
        }
    }
}

impl From<CharacterBackground> for Effect {
    fn from(value: CharacterBackground) -> Self {
        match value {
            CharacterBackground::Slave => Effect {
                hp: None,
                str: Some(-1),
                dex: None,
                int: None,
                con: Some(1),
                init: None,
            },
            CharacterBackground::Homeless => Effect {
                hp: None,
                str: Some(-1),
                dex: Some(1),
                int: None,
                con: None,
                init: None,
            },
            CharacterBackground::Farmer => Effect {
                hp: None,
                str: Some(1),
                dex: None,
                int: Some(-1),
                con: None,
                init: None,
            },
            CharacterBackground::Merchant => Effect {
                hp: None,
                str: None,
                dex: None,
                int: None,
                con: None,
                init: None,
            },
            CharacterBackground::Musician => Effect {
                hp: None,
                str: Some(-1),
                dex: Some(1),
                int: None,
                con: None,
                init: None,
            },
            CharacterBackground::Medic => Effect {
                hp: None,
                str: Some(-1),
                dex: None,
                int: Some(1),
                con: None,
                init: None,
            },
            CharacterBackground::Sailor => Effect {
                hp: None,
                str: Some(1),
                dex: Some(-1),
                int: None,
                con: None,
                init: None,
            },
            CharacterBackground::Wanderer => Effect {
                hp: None,
                str: Some(-1),
                dex: Some(1),
                int: None,
                con: None,
                init: None,
            },
            CharacterBackground::Beekeeper => Effect {
                hp: None,
                str: None,
                dex: Some(1),
                int: None,
                con: Some(-1),
                init: None,
            },
            CharacterBackground::Hunter => Effect {
                hp: None,
                str: None,
                dex: Some(1),
                int: Some(-1),
                con: None,
                init: None,
            },
            CharacterBackground::Brute => Effect {
                hp: None,
                str: Some(1),
                dex: None,
                int: Some(-1),
                con: None,
                init: None,
            },
            CharacterBackground::Assassin => Effect {
                hp: None,
                str: None,
                dex: Some(1),
                int: None,
                con: Some(-1),
                init: None,
            },
            CharacterBackground::Cultist => Effect {
                hp: None,
                str: Some(-1),
                dex: None,
                int: Some(1),
                con: None,
                init: None,
            },
            CharacterBackground::Politician => Effect {
                hp: None,
                str: None,
                dex: None,
                int: Some(1),
                con: Some(-1),
                init: None,
            },
            CharacterBackground::Blacksmith => Effect {
                hp: None,
                str: Some(1),
                dex: None,
                int: None,
                con: Some(-1),
                init: None,
            },
            CharacterBackground::Astrologist => Effect {
                hp: None,
                str: Some(-1),
                dex: None,
                int: Some(1),
                con: None,
                init: None,
            },
            CharacterBackground::Tanner => Effect {
                hp: None,
                str: None,
                dex: None,
                int: Some(-1),
                con: Some(1),
                init: None,
            },
            CharacterBackground::Scholar => Effect {
                hp: None,
                str: None,
                dex: None,
                int: Some(1),
                con: Some(-1),
                init: None,
            },
            CharacterBackground::Mercenary => Effect {
                hp: None,
                str: Some(1),
                dex: None,
                int: Some(-1),
                con: None,
                init: None,
            },
            CharacterBackground::Knight => Effect {
                hp: None,
                str: Some(1),
                dex: Some(-1),
                int: None,
                con: None,
                init: None,
            },
        }
    }
}

struct Effect {
    hp: Option<i32>,
    str: Option<i32>,
    dex: Option<i32>,
    int: Option<i32>,
    con: Option<i32>,
    init: Option<i32>,
}

impl Display for Effect {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut effects = Vec::new();
        let format_stat = |value: i32, name: &str| {
            if value > 0 {
                format!("+{} {}", value, name)
            } else {
                format!("{} {}", value, name)
            }
        };
        if let Some(str) = self.str {
            effects.push(format_stat(str, "STR"));
        }
        if let Some(dex) = self.dex {
            effects.push(format_stat(dex, "DEX"));
        }
        if let Some(int) = self.int {
            effects.push(format_stat(int, "INT"));
        }
        if let Some(con) = self.con {
            effects.push(format_stat(con, "CON"));
        }
        if let Some(hp) = self.hp {
            effects.push(format_stat(hp, "HP"));
        }
        if effects.is_empty() {
            write!(f, "()")
        } else {
            write!(f, "({})", effects.join(", "))
        }
    }
}

impl CharacterBackground {
    fn default_equipped_item(&self) -> Option<Item> {
        match self {
            CharacterBackground::Slave => Some(Item::Weapon(Weapon::Chains)),
            CharacterBackground::Homeless => Some(Item::Weapon(Weapon::Staff)),
            CharacterBackground::Farmer => Some(Item::Weapon(Weapon::Sickle)),
            CharacterBackground::Merchant => None,
            CharacterBackground::Musician => Some(Item::Weapon(Weapon::Luth)),
            CharacterBackground::Medic => Some(Item::Utility(Utility::Medikit)),
            CharacterBackground::Sailor => None,
            CharacterBackground::Wanderer => Some(Item::Armor(Armor::Hood)),
            CharacterBackground::Beekeeper => Some(Item::Weapon(Weapon::Beehive)),
            CharacterBackground::Hunter => Some(Item::Weapon(Weapon::Bow)),
            CharacterBackground::Brute => Some(Item::Weapon(Weapon::Club)),
            CharacterBackground::Assassin => Some(Item::Weapon(Weapon::Knife)),
            CharacterBackground::Cultist => Some(Item::Weapon(Weapon::Phurpa)),
            CharacterBackground::Politician => None,
            CharacterBackground::Blacksmith => Some(Item::Weapon(Weapon::Axe)),
            CharacterBackground::Astrologist => None,
            CharacterBackground::Tanner => Some(Item::Armor(Armor::Leather)),
            CharacterBackground::Scholar => None,
            CharacterBackground::Mercenary => Some(Item::Weapon(Weapon::Sword)),
            CharacterBackground::Knight => Some(Item::Armor(Armor::Chainmail)),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Item {
    Weapon(Weapon),
    Armor(Armor),
    Utility(Utility),
    Gold(usize),
}

impl Display for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Item::Weapon(weapon) => weapon.fmt(f),
            Item::Armor(armor) => armor.fmt(f),
            Item::Utility(utility) => utility.fmt(f),
            Item::Gold(count) => write!(f, "{} gold", count),
        }
    }
}

impl Item {
    fn value(&self) -> usize {
        match self {
            Item::Weapon(i) => i.value(),
            Item::Armor(i) => i.value(),
            Item::Utility(i) => i.value(),
            Item::Gold(i) => *i,
        }
    }
    fn random(rng: &mut impl RngSource) -> Self {
        match rng.gen_u32(0..=3) {
            0 => Self::Weapon(Weapon::random(rng)),
            1 => Self::Armor(Armor::random(rng)),
            2 => Self::Utility(Utility::random(rng)),
            _ => Self::Gold(rng.gen_u32(0..=20)),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Utility {
    Medikit,
    Ration,
}

impl Utility {
    fn value(&self) -> usize {
        match self {
            Utility::Medikit => 11, //write!(f, "Medikit (d6 heal)"),
            Utility::Ration => 2,   //write!(f, "Ration (resting)"),
        }
    }
    fn random(rng: &mut impl RngSource) -> Self {
        match rng.gen_u32(0..=3) {
            0 => Self::Medikit,
            _ => Self::Ration,
        }
    }
}

impl Display for Utility {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Utility::Medikit => write!(f, "Medikit (d6 heal)"),
            Utility::Ration => write!(f, "Ration (resting)"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Armor {
    Hood,
    Leather,
    Chainmail,
}

impl Armor {
    fn value(&self) -> usize {
        match self {
            Armor::Hood => 5,       //write!(f, "Hood (d2 defense)"),
            Armor::Leather => 5,    //write!(f, "Leather (d2 defense)"),
            Armor::Chainmail => 10, //write!(f, "Chainmail (d4 defense)"),
        }
    }
    fn random(rng: &mut impl RngSource) -> Self {
        match rng.gen_u32(0..=2) {
            0 => Self::Hood,
            1 => Self::Leather,
            _ => Self::Chainmail,
        }
    }
}
impl Display for Armor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Armor::Hood => write!(f, "Hood (d2 defense)"),
            Armor::Leather => write!(f, "Leather (d2 defense)"),
            Armor::Chainmail => write!(f, "Chainmail (d4 defense)"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Weapon {
    Chains,
    Staff,
    Sickle,
    Luth,
    Beehive,
    Bow,
    Club,
    Knife,
    Phurpa,
    Axe,
    Sword,
}

impl Display for Weapon {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Weapon::Chains => write!(f, "Chains (d2 + 1 attack)"),
            Weapon::Staff => write!(f, "Staff (d2 + 1 attack)"),
            Weapon::Sickle => write!(f, "Sickle (d4 attack)"),
            Weapon::Luth => write!(f, "Luth (1 attack)"),
            Weapon::Beehive => write!(f, "Beehive (d4 attack)"),
            Weapon::Bow => write!(f, "Bow (d6 attack)"),
            Weapon::Club => write!(f, "Club (d4 attack)"),
            Weapon::Knife => write!(f, "Knife (d4 attack)"),
            Weapon::Phurpa => write!(f, "Phurpa (d4 x 2 attack)"),
            Weapon::Axe => write!(f, "Axe (d6 attack)"),
            Weapon::Sword => write!(f, "Sword (d6 attack)"),
        }
    }
}

impl Weapon {
    fn value(&self) -> usize {
        match self {
            Weapon::Chains => 3,  //write!(f, "Chains (d2 + 1 attack)"),
            Weapon::Staff => 3,   //write!(f, "Staff (d2 + 1 attack)"),
            Weapon::Sickle => 6,  //write!(f, "Sickle (d4 attack)"),
            Weapon::Luth => 2,    //write!(f, "Luth (1 attack)"),
            Weapon::Beehive => 6, //write!(f, "Beehive (d4 attack)"),
            Weapon::Bow => 12,    //write!(f, "Bow (d6 attack)"),
            Weapon::Club => 6,    //write!(f, "Club (d4 attack)"),
            Weapon::Knife => 6,   //write!(f, "Knife (d4 attack)"),
            Weapon::Phurpa => 19, //write!(f, "Phurpa (d4 x 2 attack)"),
            Weapon::Axe => 12,    //write!(f, "Axe (d6 attack)"),
            Weapon::Sword => 12,  //write!(f, "Sword (d6 attack)"),
        }
    }
    fn random(rng: &mut impl RngSource) -> Self {
        match rng.gen_u32(0..=10) {
            0 => Self::Chains,
            1 => Self::Staff,
            2 => Self::Sickle,
            3 => Self::Luth,
            4 => Self::Beehive,
            5 => Self::Bow,
            6 => Self::Club,
            7 => Self::Knife,
            8 => Self::Phurpa,
            9 => Self::Axe,
            _ => Self::Sword,
        }
    }
}

impl Weapon {
    fn ranged_weapon(&self) -> bool {
        match self {
            Weapon::Chains
            | Weapon::Staff
            | Weapon::Sickle
            | Weapon::Club
            | Weapon::Knife
            | Weapon::Axe
            | Weapon::Sword => false,
            Weapon::Luth | Weapon::Beehive | Weapon::Bow | Weapon::Phurpa => true,
        }
    }
}

impl Weapon {
    fn attack(&self, rng: &mut impl RngSource, log: &mut RollLog) -> usize {
        match self {
            Weapon::Chains => Roll::D2.result(rng, log, "Attack") + 1,
            Weapon::Staff => Roll::D2.result(rng, log, "Attack") + 1,
            Weapon::Sickle => Roll::D4.result(rng, log, "Attack"),
            Weapon::Luth => 1,
            Weapon::Beehive => Roll::D4.result(rng, log, "Attack"),
            Weapon::Bow => Roll::D6.result(rng, log, "Attack"),
            Weapon::Club => Roll::D4.result(rng, log, "Attack"),
            Weapon::Knife => Roll::D4.result(rng, log, "Attack"),
            Weapon::Phurpa => {
                Roll::D4.result(rng, log, "Attack") + Roll::D4.result(rng, log, "Attack")
            }
            Weapon::Axe => Roll::D6.result(rng, log, "Attack"),
            Weapon::Sword => Roll::D6.result(rng, log, "Attack"),
        }
    }
}

impl Armor {
    fn defend(&self, rng: &mut impl RngSource, log: &mut RollLog) -> usize {
        match self {
            Armor::Hood => Roll::D2.result(rng, log, "Defense"),
            Armor::Leather => Roll::D2.result(rng, log, "Defense"),
            Armor::Chainmail => Roll::D4.result(rng, log, "Defense"),
        }
    }
}

enum Impact {
    Defend(usize),
    Attack(usize),
}

impl From<usize> for CharacterBackground {
    fn from(index: usize) -> Self {
        match index {
            1 => CharacterBackground::Slave,
            2 => CharacterBackground::Homeless,
            3 => CharacterBackground::Farmer,
            4 => CharacterBackground::Merchant,
            5 => CharacterBackground::Musician,
            6 => CharacterBackground::Medic,
            7 => CharacterBackground::Sailor,
            8 => CharacterBackground::Wanderer,
            9 => CharacterBackground::Beekeeper,
            10 => CharacterBackground::Hunter,
            11 => CharacterBackground::Brute,
            12 => CharacterBackground::Assassin,
            13 => CharacterBackground::Cultist,
            14 => CharacterBackground::Politician,
            15 => CharacterBackground::Blacksmith,
            16 => CharacterBackground::Astrologist,
            17 => CharacterBackground::Tanner,
            18 => CharacterBackground::Scholar,
            19 => CharacterBackground::Mercenary,
            20 => CharacterBackground::Knight,
            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum CharacterTrait {
    WeakMinded,
    Depressive,
    Frail,
    Clumsy,
    BadStomach,
    Alcoholic,
    EagleEye,
    Healer,
    Wise,
    Survivalist,
    Brawler,
    QuickReflex,
}

impl Display for CharacterTrait {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CharacterTrait::WeakMinded => write!(f, "Weak Minded"),
            CharacterTrait::Depressive => write!(f, "Depressive"),
            CharacterTrait::Frail => write!(f, "Frail"),
            CharacterTrait::Clumsy => write!(f, "Clumsy"),
            CharacterTrait::BadStomach => write!(f, "Bad Stomach"),
            CharacterTrait::Alcoholic => write!(f, "Alcoholic"),
            CharacterTrait::EagleEye => write!(f, "Eagle Eye"),
            CharacterTrait::Healer => write!(f, "Healer"),
            CharacterTrait::Wise => write!(f, "Wise"),
            CharacterTrait::Survivalist => write!(f, "Survivalist"),
            CharacterTrait::Brawler => write!(f, "Brawler"),
            CharacterTrait::QuickReflex => write!(f, "Quick Reflex"),
        }
    }
}

impl CharacterTrait {
    fn contradicts(&self, other: &Self) -> bool {
        matches!(
            (self, other),
            (Self::WeakMinded, Self::Wise)
                | (Self::Depressive, Self::Survivalist)
                | (Self::Frail, Self::Brawler)
                | (Self::Clumsy, Self::QuickReflex)
                | (Self::BadStomach, Self::Healer)
                | (Self::Alcoholic, Self::EagleEye)
        )
    }
}

impl From<usize> for CharacterTrait {
    fn from(value: usize) -> Self {
        match value {
            1 => Self::WeakMinded,
            2 => Self::Depressive,
            3 => Self::Frail,
            4 => Self::Clumsy,
            5 => Self::BadStomach,
            6 => Self::Alcoholic,
            7 => Self::EagleEye,
            8 => Self::Healer,
            9 => Self::Wise,
            10 => Self::Survivalist,
            11 => Self::Brawler,
            12 => Self::QuickReflex,
            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Power {
    HealingTouch,
    ShadowCloak,
    Firebold,
    Teleportation,
    SubtleKiss,
    StoneSkin,
    LifeDrain,
    FastMind,
    MindControl,
    TimePause,
    NaturesCall,
    #[allow(clippy::enum_variant_names)]
    PowerOfLife,
}

impl From<usize> for Power {
    fn from(value: usize) -> Self {
        match value {
            1 => Self::HealingTouch,
            2 => Self::ShadowCloak,
            3 => Self::Firebold,
            4 => Self::Teleportation,
            5 => Self::SubtleKiss,
            6 => Self::StoneSkin,
            7 => Self::LifeDrain,
            8 => Self::FastMind,
            9 => Self::MindControl,
            10 => Self::TimePause,
            11 => Self::NaturesCall,
            12 => Self::PowerOfLife,
            _ => panic!(),
        }
    }
}

impl Roll {
    fn result(
        &self,
        rng: &mut impl RngSource,
        log: &mut RollLog,
        description: impl AsRef<str> + 'static,
    ) -> usize {
        self.result_mute(rng, log, false, description)
    }
    fn result_mute(
        &self,
        rng: &mut impl RngSource,
        log: &mut RollLog,
        mute: bool,
        description: impl AsRef<str> + 'static,
    ) -> usize {
        let result = match self {
            Roll::D2 => rng.gen_u32(1..=2),
            Roll::D4 => rng.gen_u32(1..=4),
            Roll::D6 => rng.gen_u32(1..=6),
            Roll::D8 => rng.gen_u32(1..=8),
            Roll::D10 => rng.gen_u32(1..=10),
            Roll::D12 => rng.gen_u32(1..=12),
            Roll::D20 => rng.gen_u32(1..=20),
        };
        if !mute {
            if log
                .rolls
                .last()
                .is_some_and(|r| r.description == description.as_ref())
            {
                log.rolls.last_mut().unwrap().count += 1;
                log.rolls.last_mut().unwrap().result += result;
            } else {
                log.rolls.push(Log {
                    die: *self,
                    result,
                    description: description.as_ref().to_owned(),
                    count: 1,
                });
                if log.rolls.len() > 10 {
                    log.rolls.remove(0);
                }
            }
        }
        result
    }
}

struct RollLog {
    rolls: Vec<Log>,
}

#[derive(Debug, Clone)]
struct Log {
    die: Roll,
    result: usize,
    description: String,
    count: usize,
}

impl Display for Log {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}({}x{}) for {}",
            self.result, self.die, self.count, self.description
        )
    }
}
