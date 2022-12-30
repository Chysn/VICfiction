;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                                   Marooned
;
;                             VICfiction Story File
;                            (c) 2022, Jason Justian
;                  
; Assembled with XA
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This software is released under the Creative Commons
; Attribution-NonCommercial 4.0 International
; License. The license should be included with this file.
; If not, please see: 
;
; https://creativecommons.org/licenses/by-nc/4.0/legalcode.txt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; STORY-SPECIFIC CONFIGURATION SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;            
; Colors
SCRCOL      = 8                 ; Screen color
COL_INPUT   = 5                 ; Input color
COL_NORM    = 30                ; Text color
COL_ALERT   = 28                ; Alert color
COL_ITEM    = 158               ; Item color
COL_ROOM    = 31                ; Room name color
COL_DIR     = 159               ; Directional display color

; Story Configuration
; When the SCORE_TGT number of items are present in the room id specified
; by SCORE_ROOM, the action id specified in SCORE_ACT is triggered
SCORE_RM    = 1                 ; Score room id
SCORE_TGT   = 5                 ; Target score
SCORE_ACT   = 46                ; Action id when score is achieved

; Save Filename
SaveFile:   .asc "marooned.sav"
EON:        .asc 0              ; Here to determine length of filename

; Cardinal Directions
; In reverse order of printing
Directions: .asc 'duewsn'       ; Compass directions
AltDir:     .asc 'duspaf'       ; Maritime directions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; STORY-SPECIFIC MESSAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;            
; Text - Game Messages, Errors, etc.
Intro:      .asc "The sun beats down on you. You don't remember "
            .asc "much, but you clearly made your crew pretty mad.",LF,LF
            .asc 156,"Marooned",LF,LF,"Sample Game for VICfiction Engine",ED
NoVerbTx:   .asc COL_ALERT,"No need to do that.",ED
NoDropTx:   .asc COL_ALERT,"You don't have that.",ED
HaveItTx:   .asc COL_ALERT,"You already have it.",ED
GameOverTx: .asc COL_ALERT,"Game Over, Cap't Red!",ED
NotHereTx:  .asc COL_ALERT,"Ahr, that be not here.",ED
NoPathTx:   .asc COL_ALERT,"Nothin' be that way.'",ED
NoMoveTx:   .asc COL_ALERT,"Avast, can't get it.",ED
FullTx:     .asc COL_ALERT,"Both hands be full!",ED
ConfirmTx:  .asc COL_ALERT,"Aye aye.",ED
NoLightTx:  .asc COL_ALERT,"You can't see.",ED
UnknownTx:  .asc COL_ALERT,"Aargh, what's that?",ED
NoDirTx:    .asc COL_ALERT,"Which way?",ED
ScoreTx:    .asc COL_ALERT,"SCORE: ",ED
HelpTx:     .asc COL_ALERT,"beigemaze.com/vicfic",ED

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; INVENTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;            
; StartInv - A list of Item IDs the player has at the beginning of the game.
;            Make sure to pad this to 16 items.
; MAX_INV  - A constant specifying the NUMBER of items the player can have,
;            NOT the last inventory index.
StartInv:   .asc 0
MAX_INV     = 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; VERBS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;            
; VerbID - Cross-referenced ID list for verb synonyms
; Basic - GO (MovE), LooK (L,EX), GeT (TakE), DroP, InventorY (I), WAIT (Z)
; Extended - swim(7),dig(8),drink(9),wish(10),board(11),raise/pull(12),
;            open/unlock(13),rub(14)
Verb1:      .byte 'g','m','l','l','e','g','t','d','i','i','w','z'   ; Basic Verbs
            .byte 's','d','d','w','b','r','p','o','u','r','t',ED
                       
VerbL:      .byte 'o','e','k','l','x','t','e','p','y','i','t','z'   ; Basic Verbs
            .byte 'm','g','k','h','d','e','l','n','k','b','k'
            
VerbID:     .byte  1,  1,  2,  2,  2,  3,  3,  4,  5,  5,  6,  6    ; Basic Verbs
            .byte  7,  8,  9, 10, 11, 12, 12, 13, 13, 14,  2
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ROOMS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;            
;    D,U,E,W,S,N specify the Room ID for the room in that direction. 0 if there's
;        nothing that way
;    ActID  - Specifes the Action ID that happens whenever the player walks
;      into this room
;    RmProp - Room properties
;      Bit 0 - The room is dark. The player can't see unless carrying an item
;              with the IS_LIGHT property
;      Bit 1 - Direction Display is suppressed
;      Bit 2 - Timers (except 0) are cleared when this room is entered
;      Bit 3 - Use Alternate Direction list (maritime, perhaps)
; Room IDs are 1-indexed
Rooms:      ; Main Facility (1-3)
            ;     D, U, E, W, S, N, RmProp, DescL, DescH
            ;     D, U, S, P, A, F                          (maritime)
            .byte 0, 0, 4, 0, 2, 0, 0, <rNIsland,>rNIsland
            .byte 0, 0, 0, 3, 0, 1, 0, <rDock,>rDock
            .byte 0, 0, 2, 0, 0, 0, 0, <rWIsland,>rWIsland
            .byte 0, 0, 0, 1, 0, 0, 0, <rEIsland,>rEIsland
            .byte 0, 0, 0, 0, 0, 6, 8, <rAft,>rAft          ; Room property set
            .byte 0, 0, 0, 0, 5, 0, 8, <rFore,>rFore        ; to alt for boat

; Room Descriptions
;     The room name is terminated by ED, after which is the room description,
;     also terminated by ED
rNIsland:   .asc "Island - North",ED,'When they talk about a "deserted island," '
            .asc "they mean this.",LF,LF
            .asc "The horizon is bereft of but briny deep.",ED
rDock:      .asc "South Dock",ED,"More sea, hang the jib! But perhaps the dock "
            .asc "offers scant hope...",ED
rWIsland:   .asc "Rocky Bank",ED,"Brutal waves crash the west side. ",
            .asc "This may be the most desolate place on earth.",ED
rEIsland:   .asc "Beach",ED,"A sandy beach follows the jagged coast. If you "
            .asc "weren't doomed, you might call it paradise.",ED
rAft:       .asc "Boat, Aft",ED,"The aft section of a small wooden boat.",LF,LF
            .asc "It should suffice to get to Tortuga.",ED
rFore:      .asc "Boat, Fore",ED,"The bow of the boat. The anchor bobs "
            .asc "against the side",ED


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ITEMS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;            
;   Item1    - First Character
;   ItemL    - Last Character
;   ItemRoom - Starting Room ID
;   ItemProp - Item Properties
;     Bit 0 = Is invisible (not shown in room, but can be interacted with)
;     Bit 1 = Is un-moveable (cannot move from its room)
;     Bit 2 = Is placeholder (cannot be used as an item)
;     Bit 3 = Is timekeeping device (shows number of action attempts) 
;     Bit 4 = Is follower (item will follow player as they move)
;     Bit 5 = Is narrative (use optional third string in room narrative)
;     Bit 6 = Is scored (counts as 1 point when dropped in score room)
;     Bit 7 = Is light source (rooms with "is dark" can be seen)
;   ItemTxt  - Address of item name and description
;   (The item name is terminated by ED, after which is the item description,
;    also terminated by ED. If the item has the narrative property, a third
;    ED-terminated string will be used in the room description)
; 
; Item IDs are 1-indexed
Item1:      .byte "r","b","c","c","d","s","l","s","k","l","b","s",ED
ItemL:      .byte "m","e","t","t","n","d","p","l","y","e","t","e"
ItemRoom:   .byte  0 , 0 , 1 , 0 , 0 , 4,  0 , 3,  2,  6,  0 , 0
ItemProp:   .byte  0 , 0 , 2 , 2 ,$33, 2,  0 , 0,  0,  2, $23, 0
ItemTxtL:   .byte <iRum,<iBottle,<iChestL,<iChestO,<iDjinn,<iSand,<iLamp
            .byte <iShovel,<iKey,<iAnchor,<iBoat,<iShoe
ItemTxtH:   .byte >iRum,>iBottle,>iChestL,>iChestO,>iDjinn,>iSand,>iLamp
            .byte >iShovel,>iKey,>iAnchor,>iBoat,>iShoe

; Item Descriptions
iRum:       .asc "Bottle of RUM",ED,"Well, so your crew wasn't totally cruel.",ED
iBottle:    .asc "Empty BOTTLE",ED,"Once holding mediocre rum, now bone dry.",ED
iChestL:    .asc "Treasure CHEST",ED,'On the top is painted "See you hell, '
            .asc "Cap't Red!",'"',ED
iChestO:    .asc "Treasure CHEST",ED,"Same insult a before, but open.",ED
iDjinn:     .asc "The DJINN",ED,'"Thank you!" says the blue Djinn. "I can return '
            .asc 'the favor, if only you WISH."',ED
            .asc "The Djinn hovers nearby, his arms crossed.",ED
iSand:      .asc "Pile of SAND",ED,"It may be diggable, but not with bare hands.",ED
iLamp:      .asc "Magic LAMP",ED,"It's heavier than it appears, and is a bit "
            .asc "smudged...",ED
iShovel:    .asc "Old SHOVEL",ED,"It's decrepit, but still serviceable.",ED
iKey:       .asc "Silver KEY",ED,"This is one of your old chest keys.",ED
iAnchor:    .asc "Anchor LINE",ED,"The anchor is under water, but you can "
            .asc "easily raise the line.",ED
iBoat:      .asc "Boat",ED,"Avast, what a gorgeous sight! You can BOARD easily.",ED
            .asc "A small but sturdy boat is docked here.",ED
iShoe:      .asc "Blue SHOE",ED,"Converse All-Star size 13.",ED

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; STORY ACTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; QUALIFICATION
;---------------          
;   ActVerb    - The Verb ID for this action
;   ActItem    - The Item ID for this action. If 0, no item is used.
;   ActInRoom  - The Room ID for this action. If 0, no room is required.
;
; EVALUATION
;---------------
;   ActInvCon  - The player must be holding this Item ID for success in this
;                action. If 0, no item needs to be held.
;   ActRoomCon - The Item ID must be in this room for success in this action.
;                If 0, the action is not conditioned on this item. If both 
;                ActInvCon and ActRoomCon are non-0, then both conditions must
;                be met for the action to be successful. Note that this item may
;                be in inventory.
;   ActInvExcl - The player must NOT be holding this Item ID for success in this
;                action. If 0, no item is excluded.
; 
; RESULT
;---------------
;   ActFrom    - If the action is successful, specifies an item that can be
;                replaced with another item. The ActTo item will be set to
;                wherever the ActFrom item is, and the ActFrom item is sent
;                to room 0.
;
;                If ActFrom=0, then the player will be moved to the Room ID
;                specified in in ActTo
;
;                If bit 7 is set, no resolution is done, only the message is
;                shown.
;
;   ActTo      - If the action is successful, specifies that the item specified
;                in ActFrom will be changed to the item in ActTo. This will
;                happen in any room that the item is in, as well as the player's
;                inventory. 
;
;                ActTo can also specify a Room ID, if bit 7 is set. In this case
;                the ActFrom item will be moved directly to the specified room.
;                If ActFrom is 0, the PLAYER is moved to the specified room.
;
;                If ActTo is 0, the ActFrom item will be moved to the room that
;                the player is currently in.
;
;                If both ActFrom and ActTo are 0, then the success text is 
;                displayed and the game ends.
;
;   ActTimer   - If the action is successful, specifies that a timer will be
;                initialized or reset.
;
;                Bit 0-6 are the Timer ID. If bit 7 is set, the timer will
;                be started unless it already is running. If bit 7 is clear,
;                the timer will be disabled (set to 0).
;
;                Note that starting a timer in this way ignores TimerRoom,
;                TimerTest, and TimerTrig; the success of the action is
;                sufficient to start or stop the timer.
;
;   ActResTxt  - The address of the success and failure messasges
;                (The success message is terminated by ED, after which is the
;                 failure message, also terminated by ED)
;
; Action IDs are zero-indexed, and the action id $ff (EV) is reserved for
; actions triggered by events (timer target, enters-room, score target)
;
; Qualification
ActVerb:    .byte  13,  13,   7,   8,   9,  14,  10,  10,  11,  12,   3, EV, ED

ActItem:    .byte   3,   3,   0,   0,   1,   7,   0,   0,  11,  10,  10,  0

ActInRoom:  .byte   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  0

; Evaluation
ActInvCon:  .byte   9,   9,   0,   8,   1,   7,   0,   0,   0,   0,   0,  0

ActRoomCon: .byte   0,   0,   0,   6,   0,   0,   5,   5,   0,  10,  10,  0

ActInvExcl: .byte   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  0

; Result
ActFrom:    .byte   1,   3,  TE,   6,   1,   5,  11,   5,   0,  TE,  TE, TE

ActTo:      .byte   0,   4,  TE,   7,   2,   0, $82,  12,   5,  TE,  TE, TE

ActTimer:   .byte   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  0

ActResTxtL: .byte <aOpen,0,<aSwim,<aDig,<aDrink,<aRubLamp,<aWish,0
            .byte <aBoard,<aAnchor,<aAnchor,<aDie

ActResTxtH: .byte >aOpen,0,>aSwim,>aDig,>aDrink,>aRubLamp,>aWish,0
            .byte >aBoard,>aAnchor,>aAnchor,>aDie
            
; Action Text
; By convention, action text begins with lowercase a. The first PETSCII
; string is the text on success of conditions. The second string (after ED)
; is text on failure. If there's an action cascade, failure text only appears
; if there's no preceeding success text.
;
; DIAG is a test action to test actions without text
DIAG:       .asc "Action SUCCESS",ED,"Action FAILURE",ED
aOpen:      .asc "The chest opens with a smart CLICK. Something falls out.",ED
            .asc "Locked, ye scurvy dog!",ED
aSwim:      .asc "You desperately enter the angry surf, hoping to escape your "
            .asc "lonely prison.",LF,LF,"Within minutes, a shark finds you. Then "
            .asc "its friends find you.",ED
aDig:       .asc "You dig for hours and strike a metal object. Blimey!",ED
            .asc "It's too densely packed.",ED
aDrink:     .asc "Refreshing!",ED,"Alas, you have none.",ED
aRubLamp:   .asc "Just as the lamp is starting to get clean, the Djinn appears "
            .asc "in a blue cloud.",ED,"Nothing happens.",ED
aWish:      .asc "POOF! Your wish is granted.",ED
            .asc "If wishes were horses, we'd all be eating steak.",ED
aBoard:     .asc "You climb aboard the boat, excited about what it might mean "
            .asc "for your survival!",ED,"That's not possible.",ED
aAnchor:    .asc "Heave ho and weigh the anchor. The little boat starts "
            .asc "moving. You save vengeance for later; today you live!",ED,ED
aDie:       .asc "You die of thirst!",ED,ED
                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TIMERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;            
; TimerRoom  - The room that causes the timer to start, when entered.
; TimerInit  - The number of turns to which the timer is set when started. If
;              the value is 1, then the action is triggered immediately upon 
;              entering the room.
; TimerTrig  - How the timer is triggered when TimerRoom is entered
;              0 = Trigger on first entry
;              1 = Trigger on second and subsequent entries unless running
;              2 = Trigger on any entry unless already running
;              3 = Trigger on any entry, retrigger if already running
; TimerTest  - The Action ID that's executed when the TimerTrig condition is
;              met. If the result is SUCCESS, then the timer is started.
;              Otherwise, it is not started. If TimerTest is 0, the timer is
;              started if the TimerTrig condition is met.
; TimerAct   - The Action ID that's executed when the timer reaches 0.
;
; Memory is allocated to keep track of 48 Timers. Regardless of whether you use
; the clock (timer #1), define its parameters. If you don't use timer, set its
; TimerInit value to ED (0) as a delimiter.
;
; Timers are 1-indexed, and timer #1 is the Clock
TimerInit:  .asc 1, ED
TimerRoom:  .asc 1
TimerTrig:  .asc 0
TimerTest:  .asc 0
TimerAct:   .asc 11
TimerDir:   .asc $01 ; Timer 0 direction ($01 = +1, $ff = -1)
TimerTgt:   .asc 20  ; Timer 0 target (at which action happens)
TimerOffst: .asc 0   ; Display time offset for Timer 1
