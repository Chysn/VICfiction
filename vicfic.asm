;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                                 VICfiction
;                          Interactive Fiction Engine
;                            (c)2022, Jason Justian
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; BASIC LAUNCHER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This is the tokenization of the following BASIC program, which
; runs this game
;     42 SYS4160
* = $1001
Launcher:   .byte $0b,$10,$2a,$00,$9e,$34,$31,$30
            .byte $39,$00,$00,$00

; Memory Locations
VERB_ID     = $00               ; Verb ID
ITEM_ID     = $01               ; Item ID
PATTERN     = $02               ; Pattern (2 bytes) 
INVENTORY   = $a4               ; Inventory (2 bytes) 
CURR_ROOM   = $a6               ; Current room
END_ITEM    = $a7               ; Index of end of item list
ACT_RESULT  = $a8               ; At least one action had a result
BUFFER      = $1d00             ; Input buffer
SEEN_ROOMS  = $1d80             ; Marked as 1 when entered
ITEM_ROOMS  = $1dc0             ; RAM storage for item rooms

; Routines
PRINT       = $cb1e             ; Temporary print
CHRIN       = $ffcf             ; Get input
CHROUT      = $ffd2             ; Character out

; Constants
SPACE       = $20               ; Space
EOL         = $00               ; End of line or list
LF          = $0d               ; Linefeed
GO_CMD      = 1                 ; Basic command - GO
LOOK_CMD    = 2                 ;               - LOOK
GET_CMD     = 3                 ;               - GET
DROP_CMD    = 4                 ;               - DROP
INV_CMD     = 5                 ;               - INVENTORY
IS_VISIBLE  = $01               ; Item Property - Visible
IS_MOVEABLE = $02               ; Item Property - Moveable
SCRCOL      = 8                 ; Screen color
COL_INPUT   = 5                 ; Input color
COL_NORM    = 30                ; Text color
COL_ALERT   = 28                ; Alert color
COL_ITEM    = 158               ; Item color
COL_ROOM    = 31                ; Room name color

; Initialize
Init:       ldx #0              ; Copy initial room location data to
-loop:      inx                 ;   RAM, so it can be updated as items are   
            lda ItemRoom-1,x    ;   moved around
            sta ITEM_ROOMS-1,x  ;   ,,
            lda Item1-1,x       ; Check for last item
            bne loop            ;   If not the last item, keep going
            stx END_ITEM        ;   store end item for use as a delimiter
            lda #1              ; Initialize starting room id
            sta CURR_ROOM       ; ,,
            sta SEEN_ROOMS      ; And initialize the starting room as seen
            lda #0              ; Clear inventory
            sta INVENTORY       ; ,,
            sta INVENTORY+1     ; ,,
            ldx #$20            ; Clear the Seen Rooms list, which is used
-loop:      sta SEEN_ROOMS,x    ;   to show room details on first entry, then
            dex                 ;   suppress for subsequent entries
            bne loop            ;   ,,
            lda #COL_NORM       ; Set normal color
            jsr CHROUT          ; ,,
            lda #SCRCOL         ; Set screen color
            sta $900f           ; ,,
            lda #242            ; Set upper/lower charset
            sta $9005           ; ,,
            lda #<Intro         ; Show intro
            ldy #>Intro         ; ,,
            jsr PrintMsg        ; ,,
            ldx CURR_ROOM       ; Show room name before game starts
            jmp RoomName        ; ,,
                 
; Verb Not Found
; Show an error message, then go back for another command 
Nonsense:   lda BUFFER+1        ; Potentially process a shortcut if
            bne ShowErr         ;   only one character
            lda BUFFER          ; If the player just hit RETURN, then
            cmp #' '            ;   there's no need to show an error
            beq skip_lf         ;   ,,
shortcuts:  jmp ShortGo
ShowErr:    lda #<NoVerbTx      ; Show the error
            ldy #>NoVerbTx      ; ,,
            jsr PrintMsg        ; ,,
            ; Fall through to GetInput
            
GetInput:   jsr Linefeed
skip_lf:    lda #COL_INPUT
            jsr CHROUT
            ldx #0
-loop:      jsr CHRIN
            cmp #LF
            beq enter
            and #$7f
            sta BUFFER,x
            inx
            cpx #21
            bcc loop
enter:      jsr CHROUT
            lda #0
            sta BUFFER,x
            lda #COL_NORM
            jsr CHROUT
            jsr Linefeed
            ; Fall through to Transcribe

; Transcribe Text Buffer
;   to Verb ID and Item ID
Transcribe: ldx #0              ; Buffer index
            jsr GetPattern      ; Find the first two-character pattern
            jsr GetVerbID       ; Use the pattern to get Verb ID from database
            bcc Nonsense        ; Show an error if verb not found
            jsr GetPattern      ; Find the next two-character pattern
            jsr GetItemID       ; Use the pattern to get Item ID from database
            ; Fall through to Process

; Process Command
; Start with the Action Engine. Look through the Action Database for             
Process:    clc                 ; Clear the action result flag
            ror ACT_RESULT      ; ,,
            ldx #$ff            ; Look for actions for this command's verb
next_act:   inx                 ; ,,
            lda ActVerb,x       ; ,,
            cmp VERB_ID         ; ,,
            beq found_verb      ; ,,
            cmp #0              ; If not end of list, 
            bne next_act        ;   go back for the next action
            jmp PostAction      ; If all actions have been reviewed, next step
found_verb: lda ActItem,x       ; Get the item for this action
            beq item_ok         ;   If the action specifies no item, skip
            cmp ITEM_ID         ; Does the item match the action's item?
            bne next_act        ;   If not, go back for next action            
            jsr ItemInRm        ; Is the item in A in the current room?
            bcs item_ok         ;   If so, continue with the action attempt
NotHere:    lda #<NotHereTx     ; If the specified item isn't in the room or
            ldy #>NotHereTx     ;   inventory, show a message and go back
            jmp PrintRet        ;   for another command
item_ok:    lda ActInvCon,x     ; Is there an inventory condition?
            beq inv_ok          ;   If not, it's unconditional
            jsr ItemInv         ; Is the player holding the specified item?
            bcc failure         ;   If not, the action fails
inv_ok:     lda ActRoomCon,x    ; Is there a room condition?
            beq success         ;   If not, it's unconditional
            cmp CURR_ROOM       ; Is the player in the specified room?
            beq success         ;   If so, the action is a success
failure:    sec                 ; FAILURE!
            ror ACT_RESULT      ; Set the action result flag
            lda ActResTxtH,x    ; Show the second message associated with the
            tay                 ;   action, which is failure
            lda ActResTxtL,x    ;   ,,
            jsr PrintMsgA       ;   ,,
            jmp next_act        ;   ,,
success:    sec                 ; SUCCESS!
            ror ACT_RESULT      ; Set the action success flag
            lda ActResTxtH,x    ; Show the first message associated with the
            tay                 ;   action
            lda ActResTxtL,x    ;   ,,
            jsr PrintMsg        ;   ,,
            lda ActFrom,x       ; Now for the result. Get the From ID
            bne is_from         ;   Is there a From ID?
            lda ActTo,x         ; If there's no From ID, is there a To ID?
            bne move_pl         ;   If From=0 and To=ID then move player
game_over:  lda #<GameOverTx    ; If From=0 and To=0 then game over
            ldy #>GameOverTx    ;   Display the Game Over message,
            jsr PrintMsg        ;   ,,
forever:    jmp forever         ;   Then wait
            jmp Init            ; Start the game over
move_pl:    sta CURR_ROOM       ; Set current room specified by To ID
            jmp next_act        ; Then continue processing actions
is_from:    sta PATTERN         ; Store the From ID temporarily
            lda ActTo,x         ; Is there a To ID?
            bne xform           ;   If so, do the transform
            ldy PATTERN         ; If To=0 then move the item in From ID to
            lda CURR_ROOM       ;   the current room
            sta ITEM_ROOMS-1,y  ;   ,,
xform:      sta PATTERN+1       ; Transform - Put To where From is
            ldy PATTERN         ;   Get the From item's current location
            lda ITEM_ROOMS-1,y  ;   ,,
            ldy PATTERN+1       ;   And store it into the To index
            sta ITEM_ROOMS-1,y  ;   ,,
            ldy PATTERN         ;   Take the To item out of the system by
            lda #0              ;     setting its room to 0
            sta ITEM_ROOMS-1,y  ;     ,,
            lda INVENTORY       ; Is the From item in the left hand?
            cmp PATTERN         ;   ,,
            bne ch_inv2         ;   If not, check the other hand
            lda PATTERN+1       ;   Update the item in hand
            sta INVENTORY       ;   ,,
            cmp INVENTORY+1     ; If the same thing is in the other hand,
            bne next_act2       ;   then get rid of it
            lda #0              ;   ,,
            sta INVENTORY+1     ;   ,,
            jmp next_act        ;   Check next action
ch_inv2:    lda INVENTORY+1     ; Is the From item in the right hand?
            cmp PATTERN         ;   ,,
            bne next_act2       ;   If not, check next action
            lda PATTERN+1       ;   If so, switch
            sta INVENTORY+1     ;   ,,
            cmp INVENTORY       ; If the same thing is in the other hand
            bne next_act2       ;   then get rid of it
            lda #0              ;   ,,
            sta INVENTORY       ;   ,,
next_act2:  jmp next_act        ;   Check next action

; Perform Built-In Actions
; After the action processing is complete      
; Built-In Actions include
;   - GO
;   - LOOK
;   - GET
;   - DROP   
;   - INVENTORY
PostAction: bit ACT_RESULT
            bmi post_r
            lda VERB_ID
            cmp #GO_CMD
            bne ch_look
            jmp DoGo
ch_look:    cmp #LOOK_CMD
            bne ch_get
            jmp DoLook
ch_get:     cmp #GET_CMD
            bne ch_inv
            jmp DoGet
ch_inv:     cmp #INV_CMD
            bne ch_drop
            jmp ShowInv
ch_drop:    cmp #DROP_CMD
            beq DoDrop
            jmp Nonsense
post_r:     jmp GetInput

; Do Drop
; Of item ID
DoDrop:     ldy #1
ch_hand:    lda INVENTORY,y
            cmp ITEM_ID
            beq drop_now
            dey
            bpl ch_hand
            lda #<NoDropTx
            ldy #>NoDropTx
            jsr PrintMsg
            jmp GetInput
drop_now:   tax
            lda CURR_ROOM
            sta ITEM_ROOMS-1,x
            lda #0
            sta INVENTORY,y
            jsr Confirm
            jmp GetInput

; Do Look                        
DoLook:     lda ITEM_ID
            bne look_item
ShortRoom:  ldx CURR_ROOM
            lda RoomTxtH-1,x
            tay
            lda RoomTxtL-1,x
            jsr PrintMsgA
            jsr Linefeed
            lda #COL_ITEM
            jsr CHROUT
            lda CURR_ROOM
            ldx #0
next_room:  inx
            cpx END_ITEM
            beq show_dir
            cmp ITEM_ROOMS-1,x
            bne next_room
            pha
            lda #' '
            jsr CHROUT
            lda ItemTxtL-1,x
            ldy ItemTxtH-1,x
            jsr PrintMsg
            pla
            jmp next_room
show_dir:   lda #COL_NORM
            jsr CHROUT
            lda #'['
            jsr CHROUT
            lda #0
            sta PATTERN
            ldy CURR_ROOM
            lda RoomN-1,y
            beq add_east
            lda #'n'
            jsr CHROUT
add_east:   lda RoomE-1,y
            beq add_south
            lda #'e'
            jsr CHROUT
add_south:  lda RoomS-1,y
            beq add_west
            lda #'s'
            jsr CHROUT
add_west:   lda RoomW-1,y
            beq look_r
            lda #'w'
            jsr CHROUT
look_r:     lda #']'
            jsr CHROUT
            jsr Linefeed
            jmp GetInput                       
look_item:  jsr ItemInRm
            bcs in_room
            jmp NotHere
in_room:    tax
            lda ItemProp-1,x
            and #IS_VISIBLE
            bne ok_show
            jmp NotHere
ok_show:    lda ItemTxtH-1,x
            tay
            lda ItemTxtL-1,x
            jsr PrintMsgA
            jmp GetInput

; Do Go           
DoGo:       ldx #0
            jsr GetPattern
            jsr GetPattern
            lda PATTERN
ShortGo:    ldx CURR_ROOM
            cmp #'N'
            bne try_east
            lda RoomN-1,x
            jmp move
try_east:   cmp #'E'
            bne try_south
            lda RoomE-1,x
            jmp move
try_south:  cmp #'S'
            bne try_west
            lda RoomS-1,x
            jmp move
try_west:   cmp #'W'
            bne invalid
            lda RoomW-1,x
move:       beq go_fail
            sta CURR_ROOM
            tax
RoomName:   lda #COL_ROOM
            jsr CHROUT
            lda RoomTxtH-1,x
            tay
            lda RoomTxtL-1,x
            jsr PrintMsg
            lda #COL_NORM
            jsr CHROUT
            ldx CURR_ROOM
            lda SEEN_ROOMS-1,x
            bne return
            lda #1
            sta SEEN_ROOMS-1,x
            jmp ShortRoom
return:     jmp GetInput
invalid:    jmp ShowErr         ; Like Nonsense, but don't look at shortcuts
go_fail:    lda #<NoPathTx
            ldy #>NoPathTx
PrintRet:   jsr PrintMsg
            jmp GetInput

; Do Get            
DoGet:      lda ITEM_ID
            beq get_fail
            jsr ItemInv
            bcs have_it
            jsr ItemInRm
            bcc get_fail
            tax
            lda ItemProp-1,x
            and #IS_MOVEABLE
            beq unmoving
            lda INVENTORY
            bne ch_empty
            stx INVENTORY
            jmp got_it
ch_empty:   lda INVENTORY+1
            bne hands_full
            stx INVENTORY+1
got_it:     lda #0
            sta ITEM_ROOMS-1,x
Confirm:    lda #<ConfirmTx
            ldy #>ConfirmTx
            jmp PrintRet
get_fail:   jmp NotHere
have_it:    lda #<HaveItTx       
            ldy #>HaveItTx
            jmp PrintRet
unmoving:   lda #<NoMoveTx
            ldy #>NoMoveTx
            jmp PrintRet
hands_full: lda #<FullTx
            ldy #>FullTx
            jmp PrintRet
     
; Show Inventory       
ShowInv:    lda #COL_ITEM
            jsr CHROUT
            ldy #1
-loop:      lda INVENTORY,y
            beq nothing
            tax
            sty PATTERN
            lda #' '
            jsr CHROUT
            lda ItemTxtL-1,x
            ldy ItemTxtH-1,x
            jsr PrintMsg
            ldy PATTERN
nothing:    dey
            bpl loop
            lda #COL_NORM
            jsr CHROUT
            jmp GetInput
                                                       
; Get Pattern
;   from the Input Buffer
;   starting at position X  
GetPattern: lda BUFFER,x        ; Trim leading spaces by ignoring them
            inx                 ; ,,
            cmp #SPACE          ; ,,
            beq GetPattern      ; ,,
            sta PATTERN         ; Put the first character into the pattern
            sta PATTERN+1       ; ,,
-loop:      lda BUFFER,x        ; Get subsequent characters until we hit a
            beq pattern_r       ;   space or end of string, storing each in
            cmp #' '            ;   the second pattern byte, which will be
            beq pattern_r       ;   the final character of a verb or item
            sta PATTERN+1       ;   ,,
            inx                 ;   ,,
            lda BUFFER,x        ;   ,,
            beq pattern_r       ;   ,,
            cmp #SPACE          ;   ,,
            bne loop            ;   ,,
pattern_r:  rts

; Get Verb ID
; Go through first and last characters of the verb until
; a match is found, or return with carry clear
; Carry set indicates success, with VerbID set to index
; Verb IDs are 1-indexed
GetVerbID:  ldy #1              ; Verb index
-loop:      lda Verb1-1,y       ; Does the first character match this verb?
            cmp PATTERN         ; ,,
            bne next_verb       ; ,,
            lda VerbL-1,y       ; If so, does the last character match?
            cmp PATTERN+1       ; ,,
            beq verb_found      ; If so, store the verb id (Y), set carry
next_verb:  iny                 ; Otherwise, try the next verb
            cmp #EOL            ;   Is this the end of the list?
            bne loop            ;   If not, try next verb
            clc                 ; Set error condition
            rts                 ; At end of list, return with carry clear
verb_found: lda VerbID-1,y      ; Cross-reference VerbID to handle synonyms
            tay                 ; ,,
            sty VERB_ID         ; Set VerbID, set carry, and return
            sec                 ; ,,
            rts                 ; ,,
            
; Is Item In Room?
; Specify item ID in A  
; Carry set if in room          
ItemInRm:   tay
            lda ITEM_ROOMS-1,y
            cmp CURR_ROOM
            beq yes_in_rm
            tya
in_inv:     cmp INVENTORY
            beq yes_in_rm
            cmp INVENTORY+1
            beq yes_in_rm
            clc
            rts
yes_in_rm:  tya
            sec
            rts

; Is Item In Inventory
; Specify item ID in A  
; Carry set if in inventory          
ItemInv:    tay
            jmp in_inv

; Get Item ID
; Go through first and last characters of the item until
; a match is found, or return 0 as the ItemID
; Item IDs are 1-indexed
;
; Note that an ItemID of 0 is not necessarily an error. It just means that
; the player specified a verb only, which is often fine.   
GetItemID:  ldy #0              ; Item index
            sty ITEM_ID         ; Default (unfound) item ID
-loop:      iny                 ; Advance the index
            lda Item1-1,y       ; Get the first character of this item
            beq itemid_r        ; If end of list, return with ItemID at 0
            cmp PATTERN         ; Does the first character match?
            bne loop            ;   If not, try the next item
            lda ItemL-1,y       ; If so, does the second character match?
            cmp PATTERN+1       ;   ,,
            bne loop            ;   If not, try the next item
            tya                 ; Is the item with the found name in the
            jsr ItemInRm        ;   player's current room?
            bcc loop            ;   If not, keep looking
item_found: sty ITEM_ID         ; Item has been found. Set ItemID and return
itemid_r:   rts                 ; ,,

; Print Alternate Message
; Given the Message address (A, Y), look for the EOL+1, then print from there
PrintMsgA:  sta PATTERN
            sty PATTERN+1
            ldy #0
-loop:      lda (PATTERN),y
            inc PATTERN
            bne ch_end
            inc PATTERN+1
ch_end:     cmp #0
            bne loop
            lda PATTERN
            ldy PATTERN+1
            ; Fall through to PrintMsg

; Print Message 1
; Print the first message at the specified address
PrintMsg:   stx $fe
            jsr PRINT
            jsr Linefeed
            ldx $fe
            rts

; Linefeed shortcut
Linefeed:   lda #LF
            jmp CHROUT 
               
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TEST GAME DATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
; Verbs - By First and Last Character
;   VerbID - Cross-referenced ID list for verb synonyms
; Basic - GO (MovE), LooK (L), GeT (TakE), DroP, InventorY (I)
; Extended - OpeN, UnlocK, SummoN, TeleporT, DrinK
Verb1:      .byte "G","M","L","L","G","T","D","I","I"     ; Basic Verbs
            .byte "O","S","T","D","U",EOL
VerbL:      .byte "O","E","K","L","T","E","P","Y","I"     ; Basic Verbs
            .byte "N","N","T","K","K",EOL
VerbID:     .byte 1,1,2,2,3,3,4,5,5                       ; Basic Verbs
            .byte 6,7,8,9,6,EOL

; Items
;   Item1    - First Character
;   ItemL    - Last Character
;   ItemRoom - Starting Room ID
;   ItemProp - Item Properties
;     Bit 0 Is visible
;     Bit 1 Is moveable
;   ItemTxt  - Address of item name and description
;   (The item name is terminated by EOL, after which is the item description,
;    also terminated by EOL)
; 
; KeY, DesY (closed), DesK (open)
Item1:      .byte "K","D","D","L","C",EOL
ItemL:      .byte "Y","K","K","T","E",EOL
ItemRoom:   .byte  1 , 2 , 0 , 2 , 2 ,EOL
ItemProp:   .byte  3 , 1 , 1 , 3 , 3 ,EOL
ItemTxtL:   .byte <IKey, <IDeskC, <IDeskO, <ILocket, <ICoffee,EOL
ItemTxtH:   .byte >IKey, >IDeskC, >IDeskO, >ILocket, >ICoffee,EOL

; Rooms
;   RoomN    - Room ID to the North
;   RoomE    - Room ID to the East
;   RoomW    - Room ID to the West
;   RoomS    - Room ID to the South
;   RoomTxt  - Address of room name and description
;   (The room name is terminated by EOL, after which is the room description,
;    also terminated by EOL)
RoomN:      .byte 0, 1, 0, EOL
RoomE:      .byte 0, 0, 2, EOL
RoomS:      .byte 2, 0, 0, EOL
RoomW:      .byte 0, 3, 0, EOL
RoomTxtL:   .byte <ROffice, <RStorage, <RBreak, EOL
RoomTxtH:   .byte >ROffice, >RStorage, >RBreak, EOL

; Action Database
;   ActVerb    - The Verb ID for this action
;   ActItem    - The Item ID for this action. If 0, no item is used for the action
;   ActInvCon  - The player must be holding this Item ID for success in this
;                action. If 0, no item needs to be held.
;   ActRoomCon - The player must be in this Room ID for success in this action.
;                If 0, the action can be successful in any room. If both 
;                ActHoldCon and ActRoomCon are non-0, then both conditions must
;                be met for the action to be successful.
;   ActFrom    - If the action is successful, specifies an item that can be
;                changed to another item. If 0, then the player will be moved
;                to the Room ID specified in ActTo
;   ActTo      - If the action is successful, specifies that the item specified
;                in ActFrom will be changed to the item in ActTo. This will
;                happen in any room that the item is in, as well as the player's
;                inventory. ActTo can also specify a Room ID, if ActFrom is 0.
;                If ActTo is 0, the ActFrom item will be moved to the room that
;                the player is currently in. If both ActFrom and ActTo are 0,
;                then the text is displayed and the game ends.
;   ActResTxt  - The address of the success and failure messasges
;                (The success message is terminated by EOL, after which is the
;                 failure message, also terminated by EOL)
ActVerb:    .byte 6, 7, 8, 9, EOL
ActItem:    .byte 2, 0, 0, 5, EOL
ActInvCon:  .byte 1, 4, 4, 5, EOL
ActRoomCon: .byte 0, 2, 0, 3, EOL
ActFrom:    .byte 2, 4, 0, 0, EOL
ActTo:      .byte 3, 1, 3, 0, EOL
ActResTxtL: .byte <AOpen, <ASummon, <ATele, <ADrink, EOL
ActResTxtH: .byte >AOpen, >ASummon, >ATele, >ADrink, EOL

; Text - Game Messages, Errors, etc.
NoVerbTx:   .asc COL_ALERT,"nO NEED TO DO THAT",COL_NORM,EOL
NoDropTx:   .asc COL_ALERT,"yOU DON'T HAVE THAT",COL_NORM,EOL
HaveItTx:   .asc COL_ALERT,"yOU ALREADY HAVE IT",COL_NORM,EOL
GameOverTx: .asc COL_ALERT,"yOU win!",COL_NORM,EOL
NotHereTx:  .asc COL_ALERT,"tHAT'S NOT AROUND",COL_NORM,EOL
NoPathTx:   .asc COL_ALERT,"nOTHING'S THAT WAY",COL_NORM,EOL
NoMoveTx:   .asc COL_ALERT,"iT WON'T BUDGE",COL_NORM,EOL
FullTx:     .asc COL_ALERT,"yOUR HANDS ARE FULL",COL_NORM,EOL
ConfirmTx:  .asc COL_ALERT,"ok, SURE",COL_NORM,EOL

; Game Data
Intro:      .asc 147,"wELCOME TO vicFICTION!",LF,LF
            .asc "tHIS IS WHERE THE",LF
            .asc "INTRODUCTION GOES",LF,EOL
IKey:       .asc "sHINY key",EOL
            .asc "lOOKS LIKE IT MIGHT",LF,"FIT SOME KIND OF DESK",EOL
IDeskC:     .asc "lOCKED desk",EOL
            .asc "WHO KNOWS WHAT'S",LF,"INSIDE?",EOL
IDeskO:     .asc "oPEN desk",EOL
            .asc "hUH... eMPTY!",EOL
ILocket:    .asc "mAGIC GOLD locket",EOL
            .asc "iT CAN SUMMON AND",LF,"TELEPORT, BUT THE",LF,"GOLD IS FAKE",EOL
ICoffee:    .asc "cUP OF coffee",EOL
            .asc "iT'S STILL PRETTY HOT!",EOL
ROffice:    .asc "yOUR OFFICE",EOL
            .asc "iT'S KIND OF A SQUALID","OFFICE, WITH OPAQUE",LF,"WINDOWS",EOL
RStorage:   .asc "sTORAGE rOOM",EOL
            .asc "pAINTED A BORING GREY",LF,"WITH ALARMING",LF,"FLUORESCENT LIGHTS",EOL
RBreak:     .asc "bREAK rOOM",EOL
            .asc "tHERE'S A CUTE TABLE",LF,"PERFECT FOR COFFEE",EOL
AOpen:      .asc "tHE DESK OPENS WITH A",LF,"SMART CLICK",EOL
            .asc "iT SEEMS TO BE LOCKED",EOL
ASummon:    .asc "tHE MAGIC LOCKET",LF,"BECOMES A KEY",EOL
            .asc "nOTHING HAPPENS",EOL
ATele:      .asc "tELEPORT TO BREAK",EOL
            .asc "yOU CAN'T DO THIS",EOL
ADrink:     .asc "hITS THE SPOT!",EOL
            .asc "nOT THE RIGHT TIME",LF,"OR PLACE",EOL
  