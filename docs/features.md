# Visuals

## Visuals (Placeholders)

- [X] Toad Sprite (idle up, jump up, idle left, jump left)
- [X] Car Sprite
- [X] Sports Car Sprite
- [ ] Farm Tractor Sprite
- [X] Truck Sprite
- [X] Turtle Sprite & Animation (swimming, diving, sunk)
- [X] Croc Sprite & Animation (swimming, chomping)
- [X] Log Sprite
- [X] Toad Home Sprite

## Visuals (Good)

- [ ] Toad Sprite (idle up, jump up, idle left, jump left)
- [ ] Car Sprite
- [ ] Sports Car Sprite
- [ ] Farm Tractor Sprite
- [ ] Truck Sprite
- [ ] Turtle Sprite & Animation (swimming, diving, sunk)
- [ ] Croc Sprite & Animation (swimming, chomping)
- [ ] Log Sprite
- [ ] Toad Home Sprite
- [ ] Background art for Title Screen
- [ ] Better font for title screen

# Sound

## Sound (Placeholders)

- [ ] Game Music
- [ ] Jump Sound
- [ ] Toad Home sound
- [ ] Win Level sound
- [ ] Menu Music
- [ ] Menu Movement Sound
- [ ] Death Sound(s)

## Sound (Good)

- [ ] Game Music
- [ ] Jump Sound
- [ ] Toad Home sound
- [ ] Win Level sound
- [ ] Menu Music
- [ ] Menu Movement Sound
- [ ] Death Sound(s)

# Gameplay

## Title Screen / Menu

- [X] Title Screen w/ Text Only
- [X] Title Screen w/ Menu

## Player Movement

- [X] Tapping a direction key makes the toad "jump" in that direction. Should always travel the same distance (1 tile)
- [X] Toad cannot jump while jumping
- [X] Toad must "cooldown" after jumping
- [X] Toad cannot collide while jumping
- [X] Toad moves with a platform it is standing on

## Enemies & Obstacles

- [X] Enemies move linearly left/right. Same speed for all enemies in same row
- [X] Vehicles: Kills toad when colliding
  - [X] Car (1 tile wide)
  - [X] Sports Car (1 tile wide)
  - [ ] Farm Tractor (1 tile wide)
  - [X] Truck (2 tiles wide)
  - [ ] Turtle (1 tile wide, platform)
  - [X] Log (1 tile wide, platform)
- [X] Diving Turtle: 1 tile wide, acts as a platform when above water, does not collide when below water
- [X] Crocodile: 1 tile wide head, 2 tile wide body: Body is a platform, head is a platform when closed and death when open
- [X] Toad Home: 1 tile wide,

## Levels

- [X] Level Files
- [ ] Levels 1-5 based on original game
- [ ] "Loop Around": After finishing all levels, start from level 1 and speed everything up

## Death & Game Over

- [ ] Toad starts with 5 lives.
- [ ] Lose 1 life if toad collides with an enemy (see enemies for more details)
- [ ] Lose 1 life if toad collides with water (see terrain for more details)
- [ ] Lose 1 life if we run out of time
- [ ] Gain 1 life per 20000 score
- [X] On losing a life: Respawn at original starting position
- [ ] On 0 lives: Game over
- [ ] Unique death animations

## Score

- [/] Score for successful forward jump (10 points)
- [X] Score for making it to a toad home (50 points)
- [ ] Score for remaining time after making it to a toad home (10 points per second)
- [ ] Score for finishing a level (1000 points)
- [ ] Taking a lucky coin home (200 points)
- [ ] Catching a fly (200 points)
- [ ] On game over: Enter name for high score on victory
- [ ] View High Scores screen from Main Menu

## Control Remapping

- [X] Support Keyboard
- [ ] Support Controller
- [ ] Allow button remapping (Move Up/Down/Left/Right, Enter or Confirm, Pause or Exit)
