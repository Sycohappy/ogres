# Character Sheet V2 — Session Handoff

**Date:** 2026-07-26  
**Branch:** `feature/multi-damage-buttons`  
**Purpose:** Preserve context from the Cursor session so work can continue in a new workspace.

---

## Where we left off

1. **Shipped on this branch:** multi-damage chat buttons (tested locally via Docker).
2. **Planned next (not implemented):** Character Sheet V2 — structured JSON, resources/rest, Roll20 import rewrite, combat durations, editor UX.
3. **Reference fixtures:**
   - [`character_sheets/Argamon_flamebound.json`](../character_sheets/Argamon_flamebound.json) — hand-authored v1-style sheet (incomplete vs Roll20).
   - [`character_sheets/Argamon Flamebound _ Roll20 Characters.pdf`](../character_sheets/Argamon%20Flamebound%20_%20Roll20%20Characters.pdf) — full Roll20 export (source of truth for gaps).

**Pickup order:** Phase 1 (schema + structured rolls) + Phase 3 fixture-driven Roll20 import early; then Phase 2 resources/rest; then durations; then editor polish.

---

## Session summary (chat so far)

### 1. How damage works today (v1)

Damage is **not** live combat math against HP. It is a two-step chat dice flow:

1. Click a sheet action → optional **to-hit** (`d20 + modifier` parsed from prose).
2. Click **Damage** in chat → roll parenthesized dice like `(2d6 + 7)` from that message text.

Core logic: `src/main/ogres/app/initiative.cljs`.  
Stats, resistance, crits, and weapons are **not** recalculated at roll time — only text already on the action.

### 2. Warhammer +1 problem

Argamon’s Warhammer lists two damages (1d8+4 one-hand **or** 1d10+4 two-hand). The old Damage button parsed **both** expressions and **summed** them. The word “or” was ignored.

### 3. Feature shipped: multi-damage buttons

On `feature/multi-damage-buttons`:

- Each `(NdM + K)` gets its own chat button (labels like `1d8+4 bludgeoning`).
- Clicking a button rolls **only** that expression.
- Single-damage actions still show one **Damage** button.
- Files touched: `initiative.cljs`, `panel_chat.cljs`, `panel_chat.css`, `character_sheet_test.cljs`.
- Also fixed local Docker nginx by setting `RELEASE_VERSION` / `SERVER_SOCKET_URL` in `docker-compose.yaml`.

**Local test:** `docker compose up` → http://127.0.0.1:8080/play?r=dev  
Note: sheet pop-out is empty inside Cursor’s embedded browser; use a real browser.

### 4. Decision: move to structured sheet JSON (V2)

Agreed approach:

- **Scope (first pass focus):** structured attack / save / damage (and then full PC model).
- **Compatibility:** **dual-read** — prefer structured fields when present; fall back to prose scanning for legacy sheets.
- **Ground-up PC sheet:** not just attacks — resources like Roll20 (spell slots, long rest, durations), and a serious Roll20 import rewrite. Argamon PDF shows the current JSON is missing a lot.

### 5. Design: derived vs fundamental values

Use **layers**, not a single “store the final number” approach:

| Layer | Examples |
|--------|----------|
| **Fundamentals** | Ability **base** scores, class/level, HP roll history / override |
| **Persistent modifiers** | ASI, Amulet of Health (`set: 19` while attuned), permanent item bonuses |
| **Derived** | Effective scores, mods, natural max HP, spell DC |
| **Runtime overlays** | Aid max-HP bump, temp HP, spent slots, active effects |

- Don’t bake Amulet into the only CON value — base + modifier with source.
- Temporary spell effects belong in `runtime.effects`, not permanent vitals.
- Imports may use `maxOverride` until full level history exists.

### 6. Design: catalog vs listing everything on the sheet

**Hybrid:**

- Small **rules catalog** (class/species/background/feat templates) for “new Paladin 1” / level-up grants (e.g. Paladin hit die d10, Dragonborn Breath Weapon).
- **Sheet stores resolved copies** of features/resources/attacks with optional `source` links.
- Play/import/homebrew use the **sheet** as source of truth; catalog is for apply/sync, not live-only rendering.
- Pure catalog-only is too heavy for Ogres now (no full SRD yet). Pure sheet-only works short-term but hurts level-up consistency.

### 7. Design: do we need a new DB?

**No new server DB for now.**

- Sheets already persist for the **host** via **DataScript → IndexedDB** (`provider/state.cljs`, `provider/idb.cljs`).
- Put v2 `runtime` inside `:character-sheet/data` so spent slots/HP persist automatically.
- **Catalog** = static EDN/JSON shipped with the app.
- Add Postgres/cloud only if you need cross-device sync or accounts later.
- Export/import JSON remains a good backup story.

---

## Roll20 import gaps (current parser)

`parse-roll20` keeps roughly: name, class→type, species, alignment, AC, HP max, initiative, speed, ability scores, passive Perception, languages, resistances, attack/action **prose**.

**Dropped / unused from Argamon PDF:** skills, saving throws, proficiency bonus, spell slots & prepared spells, class/species/feats as structured features, inventory/currency/attunement, current/temp HP, hit dice remaining, death saves, inspiration, weapon masteries, free actions, background/subclass cleanly, bonus/reaction as first-class keys (folded into `:action` with suffixes).

No live resource tracking anywhere today (slots, Lay on Hands pool, concentration, duration).

---

## Character Sheet V2 plan

### Goals

1. Versioned key-value sheet (`version: 2`) — rollable data explicit.
2. Resources: spell slots, feature pools/uses, HP current/temp, hit dice — short/long rest (+ dawn).
3. Combat effects: durations + concentration during initiative.
4. Rewrite Roll20 PC PDF import → v2 (Argamon PDF as golden fixture).
5. Dual-read: structured preferred, prose fallback for v1.

Monster/bestiary markdown stays a lighter stat-block subset; PC sheets are the full model.

### Proposed v2 shape (sketch)

Keep DataScript `:character-sheet/data` as an opaque map. Example shape:

```json
{
  "version": 2,
  "identity": {
    "name": "Argamon Flamebound",
    "background": "Noble Crusader",
    "class": "Paladin",
    "subclass": null,
    "species": "Dragonborn - Red",
    "level": 6,
    "xp": { "current": 20475, "next": 23000 },
    "alignment": ["L", "E"],
    "size": "M"
  },
  "vitals": {
    "ac": { "value": 21, "from": ["Plate of Knight's Fellowship", "Shield +1"] },
    "hp": { "max": 64, "average": 64, "formula": "6d10+30" },
    "speed": { "walk": 30 },
    "initiative": { "bonus": 1 },
    "proficiencyBonus": 3,
    "passivePerception": 10,
    "senses": ["Darkvision 60 ft."]
  },
  "abilities": {
    "str": { "score": 17, "save": 6, "proficient": false },
    "cha": { "score": 16, "save": 9, "proficient": true }
  },
  "skills": {
    "athletics": { "modifier": 6, "proficient": true, "ability": "str" }
  },
  "attacks": [
    {
      "id": "warhammer-1",
      "name": "Warhammer +1",
      "kind": "attack",
      "bonus": 7,
      "damage": [
        { "id": "1h", "label": "one-handed", "count": 1, "sides": 8, "modifier": 4, "type": "bludgeoning" },
        { "id": "2h", "label": "two-handed", "count": 1, "sides": 10, "modifier": 4, "type": "bludgeoning" }
      ]
    }
  ],
  "features": [],
  "resources": [
    { "id": "lay-on-hands", "name": "Lay on Hands", "kind": "pool", "max": 25, "recharge": "long-rest" }
  ],
  "spellcasting": [],
  "inventory": {},
  "runtime": {
    "hp": { "current": 64, "temp": 0 },
    "slotsExpended": {},
    "resourceSpent": {},
    "effects": []
  }
}
```

**Rules:**

- Definition caps on `resources` / `spellcasting[].slots`; spent values under `runtime`.
- Attacks first-class; damage options array → one chat button each.
- Recharge: `long-rest` | `short-rest` | `dawn` | `manual`.
- Later: ability **base** + modifiers for items like Amulet of Health; derived effective scores/max HP.

### Phases

| Phase | Deliverable | Status |
|-------|-------------|--------|
| **1** | v2 schema, `character_sheet.cljs` adapter, structured rolls + v1 fallback, convert Argamon sample | pending |
| **2** | Spend resource/slot events, short/long rest, counters UI, sheet→token HP sync | pending |
| **3** | `parse-roll20-v2`, Argamon PDF fixture tests, stop merging bonus/reaction into action | pending |
| **4** | `runtime.effects`, concentration, initiative round expiry | pending |
| **5** | Sectioned editor + Roll20-like popout, skill/save rolls from structured mods | pending |

### Out of scope (for now)

- Full 5e auto-derivation engine from ability+PB alone  
- Full SRD/compendium drag-drop  
- Complex multiclass beyond one `spellcasting[]` entry  
- Replacing monster markdown with full PC v2  

### Primary files to touch

| Area | Files |
|------|--------|
| Schema / adapter | new `src/main/ogres/app/character_sheet.cljs`, sample JSON |
| Rolls / chat | `initiative.cljs`, `panel_characters.cljs`, `panel_chat.cljs` |
| Events | `events.cljs` |
| Import | `import/parser.cljs`, `import_test.cljs`, Argamon PDF |
| Editor / UI | `character_sheet_editor.cljs`, `panel_characters.cljs` |
| Persistence (existing) | DataScript + IndexedDB — no new DB |

---

## How to resume in a new workspace

```sh
cd /path/to/ogres
git checkout feature/multi-damage-buttons
git pull
```

1. Read this file.  
2. Start **Phase 1**: define v2 schema + adapter; keep multi-damage button behavior but feed it from structured `attacks[].damage` when present.  
3. Use Argamon PDF + JSON as fixtures for Phase 3.  
4. Local app: `docker compose up -d` → http://127.0.0.1:8080/play?r=dev (ensure `RELEASE_VERSION=dev` in compose for nginx).

### Related Cursor plan files (local machine)

- `~/.cursor/plans/character_sheet_v2_65573a9d.plan.md`  
- `~/.cursor/plans/character_sheet_v2_9cea49c1.plan.md`  

This repo doc is the portable source of truth for handoff.
