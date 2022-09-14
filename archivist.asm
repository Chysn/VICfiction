;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                            VICfiction Playground
;                     Test of VICfiction for Unexpanded VIC
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
* = $4000

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GAME-SPECIFIC CONFIGURATION SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;            
; Colors
SCRCOL      = 8                 ; Screen color
COL_INPUT   = 5                 ; Input color
COL_NORM    = 30                ; Text color
COL_ALERT   = 28                ; Alert color
COL_ITEM    = 158               ; Item color
COL_ROOM    = 31                ; Room name color
COL_DIR     = 159               ; Directional display color

; Game Configuration
; When the SCORE_TGT number of items are present in the room id specified
; by SCORE_ROOM, the action id specified in SCORE_ACT is triggered
SCORE_RM    = 1                 ; Score room id
SCORE_TGT   = 5                 ; Target score
SCORE_ACT   = 2                 ; Action id when score is achieved

; Inventory Configuration
; Make sure any inventory configured here has an ItemRoom value of 0
ST_ITEM_L   = 6                 ; Starting Item ID, left hand
ST_ITEM_R   = 0                 ; Starting Item ID, right hand 

; Timer Configuration
TIMER_START = 4                 ; Timer starting value
TIMER_DIR   = $04               ; Timer direction ($01 = +1, $ff = -1)
TIMER_TGT   = 248               ; Timer target (at which TIMEOUT_ACT happens)
TIMEOUT_ACT = 0                 ; Timeout action ID
TRIGGER     = 0                 ; Timer value when triggered
TIME_OFFSET = 9                 ; Display time offset (e.g., for clocks)

; Game Memory Locations
VERB_ID     = $00               ; Verb ID
ITEM_ID     = $01               ; Item ID
PATTERN     = $02               ; Pattern (2 bytes) 
INVENTORY   = $a4               ; Inventory (2 bytes) 
CURR_ROOM   = $a6               ; Current room
ACT_SUCCESS = $a7               ; At least one action was successful
ACT_FAILURE = $a8               ; At least one action failed
TEMP        = $a9               ; Temporary values (3 bytes)
FROM_ID     = $ac               ; From ID during transform
TO_ID       = $ad               ; To ID during transform
TIMER       = $ae               ; Game timer
RM          = $af               ; Room address (2 bytes)
SCORE       = $b1               ; Score (number of scored items in SCORE_RM)
BUFFER      = $0220             ; Input buffer
SEEN_ROOMS  = $1c00             ; Marked as 1 when entered
ITEM_ROOMS  = $1d00             ; RAM storage for item rooms

; Operating System Memory Locations
CASECT      = $0291             ; Disable Commodore case
VIC         = $9000             ; VIC chip offset
VIA1PA1     = $9111             ; VIA NMI Reset

; NMI Restore
NMINV       = $0318             ; Release NMI vector
;-NMINV     = $fffe             ; Development NMI non-vector (uncomment for dev)

; Routines
PRINT       = $cb1e             ; Temporary print
CHRIN       = $ffcf             ; Get input
CHROUT      = $ffd2             ; Character out
PRTFIX      = $ddcd             ; Decimal display routine
ISCNTC      = $ffe1             ; Check Stop key
BRK_HAND    = $fed2             ; Default BRK handler
  
; Constants
SPACE       = $20               ; Space
EOL         = $00               ; End of line or list
LF          = $0d               ; Linefeed
RVS_ON      = $12               ; Reverse on
RVS_OFF     = $92               ; Reverse off
BACKSP      = $9d               ; Backspace
CLRHOME     = $93               ; CLEAR/HOME
CRSRUP      = $91               ; Cursor Up
GO_CMD      = 1                 ; Basic Command - GO
LOOK_CMD    = 2                 ;               - LOOK
GET_CMD     = 3                 ;               - GET
DROP_CMD    = 4                 ;               - DROP
INV_CMD     = 5                 ;               - INVENTORY
IS_INVIS    = $01               ; Item Property - Invisible
IS_UNMOVE   = $02               ;               - Unmoveable
IS_PLHOLDER = $04               ;               - Placeholder
IS_TIMER    = $08               ;               - Timer Display
IS_TRIGGER  = $10               ;               - Timer Trigger
IS_GLOBAL   = $20               ;               - Usable from Anywhere
IS_SCORED   = $40               ;               - Is scored
IS_LIGHT    = $80               ;               - Light source
IS_DARK     = $01               ; Room Property - Darkened
EV          = $ff               ; Special action id for triggered action

; Initialize 
Init:       lda #<NewGame       ; Install the custom NMI (restart)
            sta NMINV           ; ,, 
            lda #>NewGame       ; ,,
            sta NMINV+1         ; ,, 
            lda #SCRCOL         ; Set screen color
            sta VIC+$0f         ; ,,
            lda VIC+$05         ; Set lowercase character set
            ora #$02            ; ,,
            sta VIC+$05         ; ,,
            lda #$80            ; Disable Commodore-Shift
            sta CASECT          ; ,,
            ; Fall through to New Game

; New Game
NewGame:    bit VIA1PA1         ; Reset NMI
            jsr ISCNTC          ; If STOP is down as the game starts
            bne start           ;   we'll go to the READY screen
            brk                 ;   ,,
start:      ldx #ST_ITEM_L      ; Initialize inventory
            stx INVENTORY       ; ,,
            ldx #ST_ITEM_R      ; ,,
            stx INVENTORY+1     ; ,,
            ldx #TIMER_START    ; Initialize timer
            stx TIMER           ; ,,            
            lda #1              ; Initialize starting room id
            jsr MoveTo          ; ,,
            ldx #0              ; Init for score and for item location copy
            stx SCORE           ; Initialize score
-loop:      inx                 ; Copy initial room location data to     
            lda ItemRoom-1,x    ;   RAM, so it can be updated as items are
            sta ITEM_ROOMS-1,x  ;   moved around
            lda Item1-1,x       ; Check for last item
            bne loop            ;   If not the last item, keep going
            ldx #$7f            ; Clear the Seen Rooms list, which is used
            lda #0              ;   to show room details on first entry, then
-loop:      sta SEEN_ROOMS,x    ;   suppress for subsequent entries
            dex                 ;   ,,
            bne loop            ;   ,,
            cli                 ; Clear SEI from ROM hardware vector
            lda #<Intro         ; Show intro
            ldy #>Intro         ; ,,
            jsr PrintMsg        ; ,,            
            jmp GameEntry       ; Show room name before game starts
                 
; Verb Not Found
; Show an error message, then go back for another command 
Nonsense:   lda BUFFER+1        ; Potentially process a shortcut if
            bne ShowErr         ;   only one character
            lda BUFFER          ; If the player just hit RETURN, then
            cmp #' '            ;   there's no need to show an error
            beq Main            ;   ,,
shortcuts:  jmp ShortGo
ShowErr:    lda #<NoVerbTx      ; Show the error
            ldy #>NoVerbTx      ; ,,
            jsr PrintMsg        ; ,,
            ; Fall through to Main
 
; Main Routine      
Main:       lda #COL_INPUT      ; Set the color
            jsr CHROUT          ; ,,
            ldx #0              ; X is buffer index
            sta BUFFER          ; Clear the buffer
-loop:      jsr CHRIN           ; KERNAL CHRIN
            cmp #LF             ; Did user press RETURN?
            beq enter           ;   If so, go to end of input
            and #$7f            ; Make case-insensitive
            sta BUFFER,x        ; Store in buffer at the index
            inx                 ; Next character
            cpx #21             ; Prevent buffer overflow
            bcc loop            ; ,,  
enter:      jsr CHROUT          ; Print the RETURN
            lda #0              ; Add the line-ending null
            sta BUFFER,x        ; ,,
            jsr NormCol         ; Set normal color
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
; matching commands and execute them.            
Process:    lda #0              ; Clear the action success and failure
            sta ACT_SUCCESS     ;   flags
            sta ACT_FAILURE     ;   ,,
            ldx #$ff            ; Look for actions for this command's verb
next_act:   inx                 ; ,,
            lda ActVerb,x       ; ,,
            bne have_verb       ; After game-specific actions, do built-in
            jmp PostAction      ; Game-specific actions are done
have_verb:  cmp VERB_ID         ; ,,
            bne next_act        ; ,,
filter_rm:  lda ActInRoom,x     ; Filter action by room
            beq get_item        ;   If unset, keep going
            cmp CURR_ROOM       ;   If the current room isn't the specified
            bne next_act        ;     room, get next action
get_item:   lda ActItem,x       ; Get the item for this action
            beq item_ok         ;   If the action specifies no item, skip
            cmp ITEM_ID         ; Does the item match the action's item?
            bne next_act        ;   If not, go back for next action            
item_ok:    jsr ItemInRm        ; Is the item in the room (or global)
            bcs ch_cond         ;   If so, start checking conditions
            jmp NotHere         ; Indicate item is not around   
ch_cond:    jsr EvalAction      ; Evaluate the action         
            jmp next_act

; Do Event
; When an event triggers an action, clear the verb and item command ids
; before performing that action, to prevent a loop of actions.           
DoEvent:    cpx #0
            bne do_event
            rts
do_event:   jsr NormCol
            lda #0
            sta ACT_SUCCESS
            sta ACT_FAILURE
            ; Fall through to EvalAction
            
EvalAction: lda ActInvCon,x     ; Is there an inventory item condition?
            beq inv_ok          ;   If not, it's unconditional
            jsr ItemInv         ; Is the player holding the specified item?
            bcc failure         ;   If not, the action fails
inv_ok:     lda ActRoomCon,x    ; Is there a room item condition?
            beq ch_excl         ;   If not, it's unconditional
            jsr ItemInRm        ; Is the item in the room or in inventory?
            bcc failure         ;   If no, the action fails
ch_excl:    lda ActInvExcl,x    ; Is there an item exclusion?
            beq success         ;   If not, the action is successful
            jsr ItemInv         ;   If so, the action is successful unless
            bcc success         ;     the item is in inventory
failure:    sec                 ; FAILURE!
            ror ACT_FAILURE     ; Set the action failure flag
            bit ACT_SUCCESS     ; If a previous success message was shown,
            bmi next_act        ;   suppress the failure message
            lda ActResTxtH,x    ; Show the failure message for the action
            beq next_act        ;   ,, (If high byte=0, it's a silent failure)
            tay                 ;   ,,
            lda ActResTxtL,x    ;   ,,
            jmp PrintAlt        ;   ,,
success:    sec                 ; SUCCESS!
            ror ACT_SUCCESS     ; Set the action success flag
            lda ActResTxtH,x    ; Show the success message for the action
            beq do_result       ;   ,, (If high byte=0, it's a silent success)
            tay                 ;   ,,
            lda ActResTxtL,x    ;   ,,
            jsr PrintMsg        ;   ,,
do_result:  lda ActFrom,x       ; Now for the result. Get the From ID
            bne is_from         ;   Is there a From ID?
            lda ActTo,x         ; If there's no From ID, is there a To ID?
            bne move_pl         ;   If From=0 and To=ID then move player
game_over:  lda #<GameOverTx    ; If From=0 and To=0 then game over
            ldy #>GameOverTx    ;   Display the Game Over message...
            jsr PrintMsg        ;   ,,
forever:    jmp forever         ; ...Then wait until RESTORE
move_pl:    jmp MoveTo          ; Set current room specified by To ID
is_from:    sta FROM_ID         ; Store the From ID temporarily
            lda ActTo,x         ; A = To ID?
            bne xform           ;   If so, do the transform
            ldy FROM_ID         ; If To=0 then move the item in From ID to
            lda CURR_ROOM       ;   the current room
            sta ITEM_ROOMS-1,y  ;   ,,
            lda ItemProp-1,y    ; If an item moved to the current room is
            and #IS_TRIGGER     ;   a trigger, then set the timer
            beq not_trig        ;   ,,
            lda #TRIGGER        ;   ,,
            sta TIMER           ;   ,,
not_trig:   rts                 ; Finish processing this action
xform:      sta TO_ID           ; Transform - Put To where From is
            cmp FROM_ID
            beq eval_r
            ldy FROM_ID         ;   Get the From item's current location
            lda ITEM_ROOMS-1,y  ;   ,,
            ldy TO_ID           ;   And store it into the To index
            sta ITEM_ROOMS-1,y  ;   ,,
            ldy FROM_ID         ;   Take the To item out of the system by
            lda #0              ;     setting its room to 0
            sta ITEM_ROOMS-1,y  ;     ,,
            lda INVENTORY       ; Is the From item in the left hand?
            cmp FROM_ID         ;   ,,
            bne ch_inv2         ;   If not, check the other hand
            lda TO_ID           ;   Update the item in hand
            sta INVENTORY       ;   ,,
            cmp INVENTORY+1     ; If the same thing is in the other hand,
            bne eval_r          ;   then get rid of it
            lda #0              ;   ,,
            sta INVENTORY+1     ;   ,,
            jmp next_act        ;   Check next action
ch_inv2:    lda INVENTORY+1     ; Is the From item in the right hand?
            cmp FROM_ID         ;   ,,
            bne eval_r          ;   If not, finish evaluation
            lda TO_ID           ;   If so, switch
            sta INVENTORY+1     ;   ,,
            cmp INVENTORY       ; If the same thing is in the other hand
            bne eval_r          ;   then get rid of it
            lda #0              ;   ,,
            sta INVENTORY       ;   ,,
eval_r:     rts                 ; Finish evaluating

; Perform Built-In Actions
; After the action processing is complete      
; Built-In Actions include
;   - GO
;   - LOOK
;   - GET
;   - DROP   
;   - INVENTORY
PostAction: bit ACT_SUCCESS     ; Bypass the basic actions if one or more
            bmi h_timer         ;   database actions was successful
            lda VERB_ID         ; Get the entered verb
            cmp #GO_CMD         ; Handle GO/MOVE
            bne ch_look         ; ,,
            jmp DoGo            ; ,,
ch_look:    cmp #LOOK_CMD       ; Handle LOOK
            bne ch_get          ; ,,
            jmp DoLook          ; ,,
ch_get:     cmp #GET_CMD        ; Handle GET/TAKE
            bne ch_inv          ; ,,
            jmp DoGet           ; ,,
ch_inv:     cmp #INV_CMD        ; Handle INVENTORY
            bne ch_drop         ; ,,
            jmp ShowInv         ; ,,
ch_drop:    cmp #DROP_CMD       ; Handle DROP
            bne no_cmd          ; ,,
            jmp DoDrop          ; ,,
no_cmd:     bit ACT_FAILURE     ; If there was an action failure, then a failure
            bmi post_r          ;   message was already shown
            jmp Nonsense        ; Error message if no match
h_timer:    jsr AdvTimer        ; Timer countdown or advance
post_r:     jmp Main            ; Return to Main main routine

; Do Drop
; Of Item ID
DoDrop:     lda ITEM_ID         ; If no Item ID was found, show an
            beq unknown         ;   error message
            ldy #1              ; Y is the hand index
ch_hand:    lda INVENTORY,y     ; Is the specified item in this hand?
            cmp ITEM_ID         ;   ,,
            beq drop_now        ;   If so, drop it
            dey                 ;   If not, check the other hand
            bpl ch_hand         ;   ,,
unknown:    lda #<NoDropTx      ; The item was not found in inventory, so
            ldy #>NoDropTx      ;   show the "don't have it" message
            jsr PrintRet        ;   ,,
drop_now:   tax                 ; X is the item index
            lda CURR_ROOM       ; Put the item in the current room
            sta ITEM_ROOMS-1,x  ; ,,
            lda #0              ; Remove it from the hand it's in
            sta INVENTORY,y     ; ,,
            jsr ShowScore       ; Show the score if a new item was dropped 
            lda SCORE           ; Have we reached the score target?
            cmp #SCORE_TGT      ; ,,
            bne drop_conf       ; ,,
            ldx #SCORE_ACT      ; If so, trigger the score action
            jsr DoEvent         ; ,,
drop_conf:  jmp Confirm         ; Show the confirmation message
drop_r:     jmp Main            ; Return to Main routine

; Do Look                        
DoLook:     jsr IsLight         ; If this is a dark room, and player has no
            bcs sees_look       ;   light source items, just show the "no light"
ShowNoSee:  lda #<NoLightTx     ;   message
            ldy #>NoLightTx     ;   ,,
            jmp PrintRet        ;   ,,
sees_look:  lda ITEM_ID         ; Get the current Item ID
            bne SingleItem      ; If there's an Item ID, then LOOK at it
            jsr RoomDesc        ; Show room desciption
            jmp Main            
            ; Look at a single item
SingleItem: jsr ItemInRm        ; The LOOK command took an Item ID. Is that item
            bcs in_room         ;   in the current room?
NotHere:    lda #<NotHereTx     ; If the specified item isn't in the room or
            ldy #>NotHereTx     ;   inventory, show a message and go back
            jmp PrintRet        ;   for another command
in_room:    tax                 ; X = specified Item ID
            lda ItemTxtH-1,x    ; Show the description of the item
            tay                 ; ,,
            lda ItemTxtL-1,x    ; ,,
            jsr PrintAlt        ; ,,
            lda ItemProp-1,x    ; If this object is a timer, show its current
            and #IS_TIMER       ;   value
            beq look_r          ;   ,,
            ldx #TIME_OFFSET    ; X is the number of "hours" in the display    
            lda TIMER           ; Get the timer value
sub_hour:   cmp #60             ; Divide by 60
            bcc lt_hour         ; ,,
            sec                 ; ,,
            sbc #60             ; ,,
            inx                 ; ,,
            bne sub_hour        ; ,,
lt_hour:    pha                 ; Show the hour display (X=low byte)
            lda #0              ; ,,
            jsr PRTFIX          ; ,,
            lda #':'            ; Colon for the timer
            jsr CHROUT          ; ,,
            pla                 ; A is the minutes part
            cmp #10             ; If < 10, pad zero
            bcs show_min        ; ,,
            pha                 ; ,,
            lda #'0'            ; ,,
            jsr CHROUT          ; ,,
            pla                 ; Get back minutes part and transfer
show_min:   tax                 ;   to X for printing
            lda #0              ; High byte is 0 (because value is 0-60)
            jsr PRTFIX          ; Print the number
            jsr Linefeed        ; Finish with linefeed
look_r:     jmp Main            ; Return to Main routine

; Do Go           
DoGo:       ldx #0              ; Starting from the first character,
            jsr GetPattern      ;   skip the first pattern (GO/MOVE)
            jsr GetPattern      ;   and get the second
ShortGo     ldy #5              ; 5 is room index for North parameter
-loop:      lda Directions,y    ; A is character for direction
            cmp PATTERN         ; Is this the attempted direction?
            beq try_move        ;   If so, try moving that way
            dey                 ; Get next direction
            bpl loop            ;   until done
            bmi invalid         ; Direction name not found
try_move:   lda (RM),y          ; Get the room id at the found index
            beq go_fail         ; Player is going an invalid direction
            jsr MoveTo          ; Set room address (RM) and CURR_ROOM            
            jsr AdvTimer        ; Timer advance or countdown on move
GameEntry:  lda #COL_ROOM       ; Always show room name after move
            jsr CHROUT          ; ,,
            jsr Linefeed        ; ,,
            jsr RoomName        ; ,,
            jsr PrintMsg        ; ,,
ch_rm_act:  ldy #6              ; Is there an entry action for this room?
            lda (RM),y          ; ,,
            beq ch_first        ; ,,
            tax                 ; If so, set action and
            jsr DoEvent         ;   attempt it
            bit ACT_SUCCESS     ; If the room action was successful, don't
            bmi go_r            ;   show anything further
ch_first:   ldx CURR_ROOM       ; Is this the first time this room has been
            lda SEEN_ROOMS-1,x  ;   visited?
            bne go_r            ; Already been visitied, so leave
            jsr RoomDesc        ; Show room description
            jmp Main         
invalid:    jmp ShowErr         ; Like Nonsense, but don't look at shortcuts
go_fail:    jsr IsLight         ; Failed to move. If there's light in the room
            bcs sees_path       ;   the player can see that they've moved
            jsr Linefeed        ; If it's dark, simply complain about it
            jmp ShowNoSee       ; ,,
sees_path:  lda #<NoPathTx      ; Show "no path" message and return to Main
            ldy #>NoPathTx      ; ,,
PrintRet:   jsr PrintMsg        ; ,,
go_r:       jmp Main    

; Do Get            
DoGet:      lda ITEM_ID
            beq get_fail
            jsr ItemInv
            bcs have_it
            jsr ItemInRm
            bcc get_fail
            tax
            lda ItemProp-1,x
            and #IS_UNMOVE
            bne unmoving
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
            sty TEMP
            lda ItemTxtL-1,x
            ldy ItemTxtH-1,x
            jsr PrintNoLF
            ldy TEMP
nothing:    dey
            bpl loop
            jmp Main    
                                                       
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
; Specify Item ID in A  
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
ch_global:  lda ItemProp-1,y
            and #IS_GLOBAL
            bne yes_in_rm
            tya
            clc
            rts
yes_in_rm:  tya
            sec
            rts

; Is Item In Inventory
; Specify Item ID in A  
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
            sty ITEM_ID         ; Default (unfound) Item ID
-loop:      iny                 ; Advance the index
            lda Item1-1,y       ; Get the first character of this item
            beq itemid_r        ; If end of list, return with ItemID at 0
            cmp PATTERN         ; Does the first character match?
            bne loop            ;   If not, try the next item
            lda ItemL-1,y       ; If so, does the second character match?
            cmp PATTERN+1       ;   ,,
            bne loop            ;   If not, try the next item
            lda ItemProp-1,y    ; If the item is a placeholder, it will not
            and #IS_PLHOLDER    ;   be used as an Item ID in a command
            bne loop            ;   ,,
            sty ITEM_ID         ; Item has been found. Set ItemID and return
itemid_r:   rts                 ; ,,

; Move To Room
; Set current room to A and add room parameter address to (RM)
MoveTo:     sta CURR_ROOM
            txa
            pha
            lda #<Rooms         ; Set starting room address
            sta RM              ; ,,
            lda #>Rooms         ; ,,
            sta RM+1            ; ,,
            ldx CURR_ROOM       ; X = room id
            dex                 ; zero-index it
            beq setaddr_r       ; if first room, (RM) is already set
-loop:      lda #10              ; Add 10 for each id
            clc                 ; ,,
            adc RM              ; ,,
            sta RM              ; ,,
            bcc nc1             ; ,,
            inc RM+1            ; ,,
nc1:        dex                 ; ,,
            bne loop            ; Multiply
setaddr_r:  pla
            tax
            rts

; Set Room Name
; Set up A and Y for room description display. This should be followed by
; PrintAlt (for name), or PrintMsg (for description)         
RoomName:   jsr IsLight
            bcs sees_name
            jmp ShowNoSee
sees_name:  ldy #8              ; 8 = desc low byte parameter
            lda (RM),y          ; A is low byte
            pha                 ; Push low byte to stack
            iny                 ; 7 = high byte parameter
            lda (RM),y          ; A is high byte
            tay                 ; Y is now high byte (for PrintMsg)
            pla                 ; A is now low byte (for PrintMsg)
            rts
             
; Advance Timer            
AdvTimer:   lda TIMER           ; If the timer is 0, then it's not active
            beq timer_r         ; ,,
            clc                 ; Add to timer
            adc #TIMER_DIR      ;   $01 for +1, $ff for -1
            sta TIMER
            cmp #TIMER_TGT      ; Has timer hit the target?
            bne timer_r         ; ,,
            pla                 ; This was called via JSR, so pull the return
            pla                 ;   address off the stack
            ldx #TIMEOUT_ACT    ; Do the specified timeout action
            jsr DoEvent         ; ,,
timer_r:    rts

RoomDesc:   jsr IsLight
            bcs sees_desc
            jmp ShowNoSee
sees_desc:  jsr NormCol         ; Otherwise look refers to a whole room
            jsr RoomName        ; Get room information
            jsr PrintAlt        ; ,,
            ldx CURR_ROOM       ; Flag the room as seen
            lda #1              ; ,,
            sta SEEN_ROOMS-1,x  ; ,,

            ; Fall through to ItemDisp

            ; Item display
ItemDisp:   jsr Linefeed
            lda #COL_ITEM       ; Set item color
            jsr CHROUT          ; ,,
            ldx #0              ; X is the item index
next_item:  inx                 ; ,,
            lda Item1-1,x       ; Has the last item been reached?
            beq DirDisp         ;   If so, show directional display
            lda CURR_ROOM                        
            cmp ITEM_ROOMS-1,x  ; Is the indexed item in the current room?
            bne next_item       ;   If not, check next item
            lda ItemProp-1,x    ; The item is in the current room, but is it
            and #IS_INVIS       ;   visible?
            bne next_item       ;   If not, check the next item
            lda ItemTxtL-1,x    ;   ,,
            ldy ItemTxtH-1,x    ;   ,,
            jsr PrintNoLF       ;   ,,
            jmp next_item       ; Go to the next item
            
            ; Directional display
DirDisp:    lda #COL_DIR
            jsr CHROUT
            lda #'('            ; Open paren
            jsr CHROUT          ; ,,
            ldy #5              ; 5 is room index for North parameter
-loop:      lda (RM),y          ; Get room id for room in this direction
            beq next_dir        ; Is there a room that way?
            lda Directions,y    ; If so, get the character
            ora #$80            ;   make it uppercase
            jsr CHROUT          ;   and print it
            lda #','
            jsr CHROUT
next_dir:   dey                 ; Get next direction
            bpl loop            ;   until done            
dir_end:    lda #BACKSP
            jsr CHROUT
            lda #')'            ; Close paren
            jsr CHROUT          ; ,,
            jsr Linefeed        ; Linefeed 
            ; Fall through to ShowScore

; Show Score
ShowScore:  ldx #0              ; X is the item index
            stx SCORE           ; And reset score
            lda CURR_ROOM       ; Score is only shown in the score room
            cmp #SCORE_RM       ; ,,
            bne score_r         ; ,,
-loop:      inx                 ; For each item
            lda Item1-1,x       ;   End of items?
            beq ch_score        ;   ,,
            lda ItemProp-1,x    ;   Is it a scored item?
            and #IS_SCORED      ;   ,,
            beq loop            ;   ,,
            lda ITEM_ROOMS-1,x  ;   Is it dropped in the current room?
            cmp #SCORE_RM       ;   ,,
            bne loop            ;   ,,  
            inc SCORE           ; Increment score if it qualifies  
            bne loop          
ch_score:   ldx SCORE
            beq score_r 
            lda #<ScoreTx
            ldy #>ScoreTx
            jsr PRINT
            ldx SCORE
            lda #0
            jsr PRTFIX
            lda #'/'
            jsr CHROUT
            ldx #SCORE_TGT
            lda #0
            jsr PRTFIX
            jsr Linefeed
score_r:    rts
 
; Is Light
; Is there light in the current room?
; Carry set if yes
IsLight:    ldy #7              ; Get room property
            lda (RM),y          ; ,,
            and #IS_DARK        ; Is this a dark room?
            beq room_light      ;   If not return with cary set
            ldy #1              ; Hand index
-loop:      lda INVENTORY,y     ; Does this hand contain a light source?
            tax                 ; ,,
            lda ItemProp-1,x    ; ,, Get item property
            and #IS_LIGHT       ; ,,
            bne room_light      ; ,,
            dey                 ; Check the other hand
            bpl loop            ; ,,
            clc                 ; No hands had a light source, so clear
            rts                 ;   carry for darkness
room_light: sec                 ; The room is well-illuminated, so set
            rts                 ;   carry for light
            
; Print Alternate Message
; Given the Message address (A, Y), look for the EOL+1, then print from there
PrintAlt:   sta PATTERN
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
PrintMsg:   pha
            tya
            pha
            jsr Linefeed
            pla
            tay
            pla
PrintNoLF:  stx TEMP+2
            jsr PRINT
            cpy #0              ; If nothing was printed, skip the
            beq print_r         ;   line feed
            jsr Linefeed        ; 
print_r:    ldx TEMP+2
            rts
 
; Normal Color Shortcut
NormCol:    lda #COL_NORM
            .byte $3c

; Linefeed Shortcut
Linefeed:   lda #LF
            jmp CHROUT 
               
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GAME DATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
Directions: .asc 'DUEWSN'

; Text - Game Messages, Errors, etc.
Intro:      .asc CLRHOME,COL_NORM,"yOU ARRIVE AT WORK",LF,"IN THE USUAL WAY, BY"
            .asc LF,"CURSOR. aFTER ALL,",LF,"YOU LIVE IN 1841. yOU",LF
            .asc "ONLY work IN 6205.",LF,LF,"yOU STRAIGHEN YOUR",LF
            .asc "GLASSES AND GLANCE AT",LF,"YOUR QUOTA SHEET.",LF,LF
            .asc "dAMN BUSY DAY AHEAD.",LF,LF,
            .asc 156,"tHE aRCHIVIST",LF,LF,"2022, jASON jUSTIAN",LF,
            .asc "bEIGE mAZE vic lAB",EOL
ScoreTx:    .asc "iNTAKE:",EOL
NoVerbTx:   .asc COL_ALERT,"nO NEED TO DO THAT",EOL
NoDropTx:   .asc COL_ALERT,"yOU DON'T HAVE THAT",EOL
HaveItTx:   .asc COL_ALERT,"yOU ALREADY HAVE IT",EOL
GameOverTx: .asc COL_ALERT,"     - the end -",EOL
NotHereTx:  .asc COL_ALERT,"tHAT'S NOT HERE",EOL
NoPathTx:   .asc COL_ALERT,"yOU CAN'T GO THAT WAY",EOL
NoMoveTx:   .asc COL_ALERT,"tHAT WON'T MOVE",EOL
FullTx:     .asc COL_ALERT,"bOTH HANDS ARE FULL",EOL
NoLightTx:  .asc COL_ALERT,"yOU CANNOT SEE",EOL
ConfirmTx:  .asc COL_ALERT,"ok",EOL

; Verbs
;   VerbID - Cross-referenced ID list for verb synonyms
; Basic - GO (MovE), LooK (L,EX), GeT (TakE), DroP, InventorY (I)
; Game - TALK(6), WIND(7), DIAL(8), SET(2), SWAP(9), BUY(10), CATCH(11)
;        OPEN(12)
; Verb IDs are 1-indexed
Verb1:      .byte 'G','M','L','L','E','G','T','D','I','I'   ; Basic Verbs
            .byte 'T','W','D','R','S','B','C','O',EOL
VerbL:      .byte 'O','E','K','L','X','T','E','P','Y','I'   ; Basic Verbs
            .byte 'K','D','L','D','P','Y','H','N'
VerbID:     .byte  1,  1,  2,  2,  2,  3,  3,  4,  5,  5    ; Basic Verbs
            .byte  6,  7,  8,  2,  9, 10, 11, 12

; Rooms
; Room IDs are 1-indexed
Rooms:      ; Main Facility (1-3)
            ;     D, U, E, W, S, N, ActID, RmProp, DescL, DescH
            .byte 2, 0, 0, 0, 0, 0, 0, 0, <rIntake,>rIntake
            .byte 0, 1, 3, 0, 0, 0, 0, 0, <rOffice,>rOffice
            .byte 0, 0, 0, 2, 0, 0, 2, 0, <rPlaza,>rPlaza
            
            ; Graff House, 1776 (4-8)
            ;     D, U, E, W, S, N, ActID, RmProp, DescL, DescH
            .byte 0, 0, 5, 0, 0, 0, 0, 0, <rCorner,>rCorner
            .byte 0, 7, 0, 4, 0, 0, 0, 0, <rFoyer,>rFoyer
            .byte 0, 0, 0, 0, 0, 7, 0, 0, <rJeffRoom,>rJeffRoom
            .byte 5, 0, 0, 0, 6, 8, 0, 0, <rLanding,>rLanding
            .byte 0, 0, 0, 0, 7, 0, 0, 0, <rJeffBed,>rJeffBed
            
            ; Navin Field, 1934 (9-15)
            ;     D, U, E, W, S, N, ActID, RmProp, DescL, DescH
            .byte 0, 0, 0, 0, 0,14, 0,  0, <rTBooth,>rTBooth
            .byte 0, 0,11,13,14, 0, 18, 0, <rHomeSt,>rHomeSt
            .byte 0, 0, 0,12,10, 0, 14, 0, <rRightF,>rRightF
            .byte 0, 0,11,13,10, 0, 18, 0, <rCenterF,>rCenterF
            .byte 0, 0,12, 0,10, 0, 0,  0, <rLeftF,>rLeftF
            .byte 0, 0, 0, 0, 9,10, 11, 0, <rCorridor,>rCorridor
            .byte 0, 0,15,15, 0, 0, 0,  0, <rJail,>rJail
            
            ; Nefertari's Tomb, 1256BC (16-22)
            ;     D, U, E, W, S, N, ActID, RmProp, DescL, DescH
            .byte 0, 0,17, 0, 0, 18, 0, 1, <rAnteCh,>rAnteCh
            .byte 0, 0, 0,16, 0, 0 , 0, 1, <rSideCh,>rSideCh
            .byte 19,0, 0, 0, 16,0 , 0, 1, <rRamp,>rRamp
            .byte 0,18,21,20, 0, 22, 0, 1, <rSarcRm,>rSarcRm
            .byte 0,0, 19, 0, 0, 0 , 0, 1, <rWAnnex,>rWAnnex
            .byte 0,0,  0,19, 0, 0 , 0, 1, <rEAnnex,>rEAnnex
            .byte 0,0,  0, 0,19, 0 , 0, 1, <rResOs,>rResOs

; Room Descriptions
;     The room name is terminated by EOL, after which is the room description,
;     also terminated by EOL
rIntake:    .asc "iNTAKE rOOM",EOL,"tHIS CIRCULAR ROOM IS",LF
            .asc "YOUR WORK AREA. tHE",LF,"CURSOR DOMINATES THE",LF
            .asc "EXACT CENTER OF THE",LF,"SPACE. tHE CONSOLE IS",LF
            .asc "ABOUT A METER AWAY.",LF,LF,"a LADDER TUBE LEADS",LF
            .asc "DOWN TO aDMIN.",EOL
rOffice:    .asc "bOSS'S oFFICE",EOL,"tHE bOSS ISN'T ALWAYS",LF
            .asc "IN, BUT SHE IS TODAY,",LF,"AND SHE GIVES YOU A",LF
            .asc "SLIGHT NOD AS YOU",LF,"EXIT THE LADDER.",LF,LF
            .asc "'dAMN BUSY DAY,",LF,"TODAY,' SHE SAYS.",LF,LF
            .asc "tHE DOOR TO THE EAST",LF,"IS MARKED WITH A HUGE",LF
            .asc "RED EXLAMATION POINT.",EOL
rPlaza:     .asc "pLAZA",EOL,EOL

            ; Graff House, 1776
rCorner:    .asc "cORNER",EOL,"tHE CORNER OF 7TH AND",LF,"mARKET sTREET IN",LF
            .asc "pHILADELPHIA.",LF,LF,"a NEW HOUSE WITH AN",LF
            .asc "INTRICATE fLEMISH",LF,"bOND BRICK PATTERN",LF
            .asc "ADORNS THE CORNER.",LF,LF,"tHERE'S AN ENTRYWAY",LF
            .asc "ON THE WEST FRONT.",EOL
rFoyer:     .asc "fOYER, gRAFF hOUSE",EOL,"tHE WHOLE OF gRAFF",LF
            .asc "hOUSE IS CLEARLY",LF,"NEW CONSTRUCTION,",LF
            .asc "LOVINGLY DESIGNED AND",LF,"BUILT BY ITS OCCUPANT",LF
            .asc "AND LANDLORD.",LF,LF
            .asc "tHERE'S A STAIRWAY",LF,"UP, BUT THE REST OF",LF
            .asc "THE HOUSE IS LOCKED",LF
            .asc "OFF.",EOL            
rLanding:   .asc "lANDING",EOL,"tHERE ARE ROOMS TO",LF,"THE NORTH AND SOUTH.",EOL
rJeffRoom:  .asc "pARLOR",EOL,"sOMEBODY IS DOING A",LF,"LOT OF WRITING HERE.",LF
            .asc "pAPERS ARE STREWN",LF,"AROUND, ANYTHING",LF
            .asc "LINEN IS STAINED BY",LF,"INK. a WASTEBASKET",LF
            .asc "BRIMS WITH REJECTED",LF,"DRAFTS READING 'iN",LF
            .asc "cONGRESS.'",EOL   
rJeffBed:   .asc "bED cHAMBER",EOL,"tHE BED IS NEATLY",LF
            .asc "MADE AND TINY.",LF,"wHOEVER RENTS THIS",LF
            .asc "ROOM DOESN'T SLEEP",LF,"MUCH.",EOL
rTBooth:    .asc "tICKET bOOTH",EOL,"nAVIN fIELD, dETROIT.",LF,LF
            .asc "tODAY THE tIGERS ARE",LF,"PLAYING THE yANKEES",LF
            .asc "WITH bABE rUTH. tHIS",LF,"IS A SPECIAL DAY.",LF,LF
            .asc "a FRECKLY KID AT THE",LF,"COUNTER IS BARKING,",LF
            .asc "'tICKETS! bUY YOUR",LF,"TICKETS HERE!'",EOL
rHomeSt:    .asc "hOME pLATE sTANDS",EOL,"tHE GREEN OF THE",LF
            .asc "DIAMOND IS SOMETHING",LF,"YOU'LL NEVER FORGET,",LF
            .asc "AS ARE THE SOUNDS OF",LF,"THE BAT AND THE SMELL",LF
            .asc "OF THE ALMONDS.",LF,LF,"bUT YOU DON'T WANT TO",LF
            .asc "BE BEHIND THE PLATE.",LF,LF,"lEFT fIELD IS TO THE",LF
            .asc "WEST, AND rIGHT fIELD",LF,"TO THE EAST.",EOL
rRightF:    .asc "rIGHT fIELD sTANDS",EOL,"tHE CROWD ROARS.",EOL
rCenterF:   .asc "cENTER fIELD sTANDS",EOL,"tHE CROWD OUT HERE IS",LF
            .asc "RAUCOUS, EVEN FOR",LF,"dETROIT. yOU'RE",LF
            .asc "ENJOYING THE",LF,"ATMOSPHERE BUT YOU",LF
            .asc "KNOW bAM HIT TO",LF,"RIGHT.",EOL
rLeftF:     .asc "lEFT fIELD sTANDS",EOL,"eVERYBODY IS",LF
            .asc "ANTICIPATING THE BIG",LF,"MOMENT. kIDS WITH",LF
            .asc "GLOVES PACK THE",LF,"STANDS. yOU KNOW",LF
            .asc "THEY'LL SOON BE SAD",LF,"ABOUT BEING ON THE",LF
            .asc "WRONG SIDE OF THE",LF,"PARK.",EOL
rCorridor:  .asc "cORRIDOR",EOL,"sTANDS ARE TO THE",LF,"NORTH.",EOL
rJail:      .asc "dETROIT jAIL",EOL,"tHE CELL IS LIKE 2x2",LF
            .asc "METERS. iT'S SUPER",LF,"EMBARASSING.",LF,LF
            .asc "hOPEFULLY YOU HAVE",LF,"YOUR REEL.",EOL
rAnteCh:    .asc "aNTECHAMBER",EOL,"tOMB OF nEFERTARI,",LF
            .asc "vALLEY OF THE qUEENS,",LF,"lUXOR.",LF,LF
            .asc "tHE SMARTPHONE HAS",LF,"ENOUGH ILLUMINATION",LF
            .asc "FOR YOU TO FIND YOUR",LF,"WAY, AND FOR YOU TO",LF
            .asc "APPRECIATE THE RICHLY",LF,"COLORED ILLUSTRATIONS",LF
            .asc "THAT COVER THE WALLS.",EOL
rSideCh:    .asc "sIDE cHAMBER",EOL,"tO THE RIGHT OF THE",LF
            .asc "aNTECHAMBER IS THE",LF,"MUCH SMALLER sIDE",LF
            .asc "cHAMBER. cOLORFUL",LF,"ILLUSTRATIONS COVER",LF
            .asc "EACH WALL,A DIFFERENT",LF,"STYLE ON EVERY",LF
            .asc "SURFACE.",EOL
rRamp:      .asc "rAMP",EOL,"tHIS DARK, STEEP RAMP",LF,"HEADS DOWN TO A LARGE"
            .asc LF,"CHAMBER.",EOL
rSarcRm:    .asc "sARCOPHAGUS rOOM",EOL,"hELLO",EOL
rWAnnex:    .asc "wESTERN aNNEX",EOL,EOL
rEAnnex:    .asc "eASTERN aNNEX",EOL,EOL
rResOs:     .asc "rESIDENCE OF oSIRIS",EOL,EOL
            
; Items
;   Item1    - First Character
;   ItemL    - Last Character
;   ItemRoom - Starting Room ID
;   ItemProp - Item Properties
;     Bit 0 = Is invisible (not shown in room, but can be interacted with)
;     Bit 1 = Is un-moveable (cannot move from its room)
;     Bit 2 = Is placeholder (cannot be used as an item)
;     Bit 3 = Is timekeeping device (shows number of action attempts) 
;     Bit 4 = Is trigger for timer (when this item is moved to a room, the
;             timer starts)
;     Bit 5 = Is global (can be interacted with from any room)
;     Bit 6 = Is scored (counts as 1 point when dropped in score room)
;     Bit 7 = Is light source (rooms with "is dark" can be seen)
;   ItemTxt  - Address of item name and description
;   (The item name is terminated by EOL, after which is the item description,
;    also terminated by EOL)
; 
; Item IDs are 1-indexed
Item1:      .byte 'C','C','B','R','Q','W','1','D','1','S','*','J','B'
            .byte 'T','C','1','G','*','*','*','1','S','P','I',EOL
ItemL:      .byte 'R','E','S','L','A','H','6','K','1','E','*','N','L'
            .byte 'T','N','4','E','*','*','*','C','S','E','S'
ItemRoom:   .byte  1 , 1,  2 , 2 , 1 , 0,  1 , 6 , 1 , 1 , 6 , 0 , 0
            .byte  0 , 8,  1 ,13 ,11 , 0,  0 , 1 ,19 ,19 , 0
ItemProp:   .byte  3 , 3,  3 , 0, 0 , 8,  3 ,$40,  3 ,$80, 7 , 2 ,$40
            .byte  0 , 0,  3 , 1 , 7 , 7,  7 , 3,  2 , 0 ,35
ItemTxtL:   .byte <iCursor,<iConsole,<iBoss,<iReel,<iQuota,<iWatch,<iYear
            .byte <iDesk,<iYear,<iPhone,0,<iJefferson,<iBall,<iTicket
            .byte <iSixpence,<iYear,<iGlove,0,0,0,<iYear,<iSarc,<iPlaque
            .byte <iIllus
ItemTxtH:   .byte >iCursor,>iConsole,>iBoss,>iReel,>iQuota,>iWatch,>iYear
            .byte >iDesk,>iYear,>iPhone,0,>iJefferson,>iBall,>iTicket
            .byte >iSixpence,>iYear,>iGlove,0,0,0,>iYear,>iSarc,>iPlaque
            .byte >iIllus

; Item Descriptions
iCursor:    .asc "cURSOR",EOL,"tHE CURSOR LOOKS LIKE",LF
            .asc "A STEAM ENGINE",LF,"STUFFED IN A TUXEDO.",LF
            .asc "sEVERAL WHEELS SPIN",LF,"AT VARYING SPEEDS.",LF
            .asc "tUBES AND VALVES ARE",LF,"EVERYWHERE.",LF,LF
            .asc "tHE bOSS TRIED TO",LF,"EXPLAIN HOW IT WORKS",LF
            .asc "ONCE. hAD TO DO WITH",LF,"BUBBLES. iT GETS YOU",LF
            .asc "FROM WHEN TO WHEN.",EOL
iConsole:   .asc "cONSOLE",EOL,"yOU'D THINK THEY'D",LF
            .asc "HAVE FANCIER CONSOLES",LF,"IN THE 63RD CENTURY,",LF
            .asc "BUT THIS CONSOLE IS",LF,"JUST A STAND WITH A",LF
            .asc "ROTARY CONTROL TO",LF,"dial DESTINATIONS.",EOL
iBoss:      .asc "tHE boss",EOL,"tHE bOSS LOOKS LIKE A",LF
            .asc "MIDDLE-AGED WOMAN",LF,"IN A VIOLET LAB COAT,",LF
            .asc "BUT YOU KNOW SHE'S",LF
            .asc "BEEN DOING THIS FOR A",LF,"HUNDRED YEARS. sHE'S",LF
            .asc "WORKING MENTALLY AT",LF,"THE MOMENT, BUT YOU",LF
            .asc "KNOW YOU CAN ALWAYS",LF,"talk TO HER.",EOL
iReel:      .asc "tEMPORAL reel",EOL,"tHE REEL IS THE",LF
            .asc "REMOTE COMPONENT OF",LF,"THE CURSOR. iT'S A",LF
            .asc "PALM-SIZED WHEEL WITH",LF,"A PARALLEL SMALLER",LF
            .asc "WHEEL AFFIXED, SOFTLY",LF,"WHIRRING, WITH A",LF
            .asc "PULSING AMBER LIGHT.",LF,LF
            .asc "yOU OPERATE IT BY",LF,"windING IT.",EOL
iQuota:     .asc "quota SHEET",EOL,"--dUE tODAY--",LF,LF," * 1776",LF
            .asc " * 1934",LF," * 2022",LF," * 1255bc",LF," * 23",LF,LF
            .asc "cHERNOV COLLECTS",LF,"YOUR iNTAKE AT 17:00.",LF,
            .asc "yOU JUST NEED TO drop",LF,"ASSETS IN THIS ROOM.",EOL
iWatch:     .asc "pOCKET watch",EOL,"18TH cENTURY. a GIFT",LF
            .asc "FROM dAD. oRNATE.",EOL
iYear:      .asc "jUST A YEAR",EOL,"dial THE YEAR INTO",LF,"THE CONSOLE.",EOL
iDesk:      .asc "jEFFERSON'S desk",EOL,"tHIS IS THE DESK THAT",LF
            .asc "tHOMAS jEFFERSON IS",LF,"USING TO WRITE THE",LF
            .asc "dECLARATION OF",LF,"iNDEPENDENCE. iF IT",LF
            .asc "GOES MISSING, HE'LL",LF,"WRITE IT ON SOMETHING",LF
            .asc "ELSE.",LF,LF,"iF IT SEEMS THERE'S A",LF
            .asc "PARADOX HERE, THAT'S",LF,"cHERNOV'S PROBLEM.",EOL
iPhone:     .asc "smartphone",EOL,"nOT IN THE LEAST",LF,"ANACHRONISTIC, IT'S"
            .asc LF,"A 150MM BY 75MM SLAB",LF,"WITH A BRIGHT SCREEN.",LF,LF
            .asc "zERO BARS.",EOL
iJefferson: .asc "tHOMAS jefferson",EOL,"yES, that jEFFERSON.",EOL
iBall:      .asc "rUTH'S hOME rUN ball",EOL,"bABE rUTH HIT HIS",LF
            .asc "700TH HOME RUN WITH",LF,"THIS BASEBALL.",EOL
iTicket:    .asc "bASEBALL ticket",EOL,"jULY 14, 1934 tIGERS",LF
            .asc "VS. yANKEES $1.40",EOL
iSixpence:  .asc "sIXPENCE coin",EOL,"aN OLD bRITISH COIN,",LF
            .asc "WORTHLESS NOW.",EOL
iGlove:     .asc "bASEBALL glove",EOL,"iT'S A TAD SMALL,",LF
            .asc "AS A CHILD'S GLOVE,",LF,"BUT IT SHOULD WORK.",EOL
iPlaque:    .asc "iNSCRIBED gOLD plaque",EOL,"yOUR ANCIENT eGYPTIAN",LF,
            .asc "IS RUSTY... mAYBE:",LF,LF,COL_ALERT
            .asc "take no  gold  from my",LF,"birthright,  lest  its",LF
            .asc "curse snuff your light",LF
            .asc "(silver & bronze too!)",LF,COL_NORM
            .asc "eGYPTIAN MUMMY CURSE?",LF,"that CAN'T BE A REAL",LF
            .asc "THING. rIGHT...?",EOL
iSarc:      .asc "sarcophagus",EOL,"nEFERTARI'S FINAL",LF
            .asc "RESTING PLACE IS",LF,"AMAZING, LIKE",LF
            .asc "EVERYTHING HERE.",LF,"sTUNNING ROSE",LF
            .asc "GRANITE, INLAID WITH",LF,"GOLD AND CARNELIAN.",LF
            .asc "tHE qUEEN'S CARVED",LF,"FACE LOOKS HOPEFULLY",LF
            .asc "AT THE CEILING.",EOL
iIllus:     .asc EOL,"tHEY JUST CAN'T BE",LF,"ADEQUATELY EXPRESSED",LF
            .asc "IN THIS MEDIUM. iF",LF,"YOU'RE STUCK IN THE",LF
            .asc "21ST CENTURY, TRY",LF,"gOOGLE.",EOL

; Actions
;   ActVerb    - The Verb ID for this action
;   ActItem    - The Item ID for this action. If 0, no item is used.
;   ActInRoom  - The Room ID for this action. If 0, no room is required.
;   ActInvCon  - The player must be holding this Item ID for success in this
;                action. If 0, no item needs to be held.
;   ActRoomCon - The Item ID must be in this room for success in this action.
;                If 0, the action is not conditioned on this item. If both 
;                ActHoldCon and ActRoomCon are non-0, then both conditions must
;                be met for the action to be successful. Note that this item may
;                be in inventory.
;   ActInvExcl - The player must NOT be holding this Item ID for success in this
;                action. If 0, no item is excluded.
;   ActFrom    - If the action is successful, specifies an item that can be
;                changed to another item.
;
;                If 0, then the player will be moved to the Room ID specified 
;                in ActTo
;   ActTo      - If the action is successful, specifies that the item specified
;                in ActFrom will be changed to the item in ActTo. This will
;                happen in any room that the item is in, as well as the player's
;                inventory. 
;
;                ActTo can also specify a Room ID, if ActFrom is 0.
;
;                If ActTo is 0, the ActFrom item will be moved to the room that
;                the player is currently in.
;
;                If both ActFrom and ActTo are 0, then the text is displayed and 
;                the game ends.
;
;                If both ActFrom and ActTo are the same non-zero Item ID, only
;                messages will be displayed.
;   ActResTxt  - The address of the success and failure messasges
;                (The success message is terminated by EOL, after which is the
;                 failure message, also terminated by EOL)
;
; Action IDs are zero-indexed, and the action id $ff (EV) is reserved for
; actions triggered by events (timer target, enters-room, score target)
ActVerb:    .byte 6,7,EV,8,8,3, 3,  6, 9,  9, 9,EV, 8,10,EV,11,11,11
            .byte EV, 8,12,EOL
ActItem:    .byte 3,4,0, 7,9,8, 8, 12,10,  4, 0,0 ,16,14, 0,13,13,13
            .byte 0, 21,22
ActInRoom:  .byte 0,0,0, 0,0,0, 0,  0, 0,  0, 0,0,  0, 0, 0,11,11,11
            .byte 0,  0, 0
ActInvCon:  .byte 0,4,0, 0,0,0, 0,  0,10,  4, 0,0,  0,15, 0, 0,17, 0
            .byte 13, 0, 0
ActRoomCon: .byte 3,0,0, 1,1,11,12, 0,12, 12,12,0 , 1, 0,18,20,19,19
            .byte 0,  1, 0
ActInvExcl: .byte 0,1,0, 0,0,8, 8,  8, 8,  8, 8,14, 0, 0, 0,0,  0, 0
            .byte 0,  0, 0
ActFrom:    .byte 1,0,0, 0,0,11,1,  1, 10, 4, 1,0 , 0,15,18,1, 17,19
            .byte 0,  0, 1
ActTo:      .byte 1,1,0, 4,0,12,1,  1, 8,  8, 1,9 , 9,14,19,1, 13,20
            .byte 15,16, 1
ActResTxtL: .byte <aBoss,<aHome,<aDie,<aX,<a1841,<aJeffEnter,<aJeffSay
            .byte <aJeffOffer,<aJeffAcc,<aJeffAcc,<aJeffDecl
            .byte <aNeedTix,<aX,<aBuyTix,<aBallHit,<aMissed,<aTryCatch,0
            .byte <aToJail,<aX,<aOpSarc
ActResTxtH: .byte >aBoss,>aHome,>aDie,>aX,>a1841,>aJeffEnter,>aJeffSay
            .byte >aJeffOffer,>aJeffAcc,>aJeffAcc,>aJeffDecl
            .byte >aNeedTix,>aX,>aBuyTix,>aBallHit,>aMissed,>aTryCatch,0
            .byte >aToJail,>aX,>aOpSarc
            
; Action Results
aBoss:      .asc "'hAVE A GREAT DAY,",LF,"AND DON'T FORGET YOUR",LF
            .asc "REEL!'",EOL,"sHE'S NOT HERE.",EOL
aHome:      .asc CLRHOME,"bEING REELED BACK IS",LF,"ALWAYS DISCONCERTING.",LF
            .asc "iT'S LIKE RIDING A",LF,"ROLLER COASTER WHILE",LF
            .asc "WEARING A vr HEADSET",LF,"OF A DIFFERENT ROLLER",LF
            .asc "COASTER.",LF,LF,"tHE SENSATION LASTS",LF
            .asc "ONLY A MOMENT AND",LF,"YOU'RE BACK TO YOUR",LF
            .asc "iNTAKE rOOM.",EOL
            .asc "yOU DON'T HAVE A",LF,"TEMPORAL REEL.",EOL
aDie:       .asc CRSRUP,CRSRUP,CRSRUP,CRSRUP,"tHE bOSS RUSHES TO",LF
            .asc "TACKLE YOU BUT IT'S",LF,"TOO LATE. yOU NOTICE",LF
            .asc "A MAGNIFICENT FUTURE",LF,"CITYSCAPE FOR ONLY A",LF
            .asc "MOMENT BEFORE THE",LF,"bUBBLE COLLAPSES",LF
            .asc "AROUND YOU AND YOU",LF,"STOP EXISTING.",EOL,EOL
aX:         .asc CLRHOME,"yOU DIAL THE YEAR ON",LF,"THE CONSOLE.",LF,LF
            .asc "tHE CURSOR'S WHEELS",LF,"SPIN FASTER. yOU FEEL",LF
            .asc "A HOT LOUD RUSH OF",LF,"AIR LIKE A LOCOMOTIVE",LF
            .asc "IS GOING THROUGH YOUR",LF,"CHEST.",LF,LF
            .asc "yOUR SURROUNDINGS",LF,"HAVE CHANGED...",EOL
            .asc "tHERE'S NO CURSOR.",EOL
a1841:      .asc "tHE CURSOR ROARS YOU",LF,"BACK HOME. yOU SMELL",LF
            .asc "THE FAMILIAR SMELLS",LF,"OF YOUR COMFORTING",LF
            .asc "HEARTH AND RELAX...",LF,LF
            .asc "...bUT THEN YOU",LF,"THINK OF THE CURSOR",LF
            .asc "IN YOUR DEN. tHE bOSS",LF
            .asc "WON'T BE HAPPY",LF,"ABOUT YOUR LEAVING",LF
            .asc "EARLY.",LF,LF,"tOMORROW'S GOING TO",LF
            .asc "BE A DAMN BUSY DAY.",EOL,"tHERE'S NO CONSOLE.",EOL
aJeffEnter: .asc "jUST AS YOU TAKE THE",LF,"LITTLE DESK, A TALL",LF
            .asc "MAN EMERGES FROM",LF,"BEHIND AND WRESTS IT",LF
            .asc "FROM YOUR HANDS.",EOL,EOL
aJeffSay:   .asc "'i DON'T THINK THIS",LF,"BELONGS TO YOU,'",LF
            .asc "jEFFERSON FROWNS.",EOL,EOL
aJeffOffer: .asc "'i INVENTED THIS DESK",LF,"AND i'M NOT GOING TO",LF
            .asc "LET YOU TAKE IT.",LF,LF,"'bUT... yOU LOOK LIKE",LF
            .asc "SOMEONE WITH ACCESS",LF,"TO GREAT FUTURISTIC",LF
            .asc "INVENTIONS. iF YOU",LF,"BRING ME SOMETHING",LF
            .asc "FROM THE FUTURE, i'LL",LF,"swap IT FOR MY",LF
            .asc "EXCELLENT DESK!'",EOL,"'i BID YOU GOOD DAY.'",EOL
aJeffDecl:  .asc "'i HAVE ABSOLUTELY NO",LF,"INTEREST IN THAT.'",EOL
            .asc "nOBODY TO SWAP WITH!",EOL
aJeffAcc:   .asc "'wHAT A MARVELOUS",LF,"THING! i DARESAY WE",LF
            .asc "HAVE A DEAL.",LF,LF,"'i'LL HAVE A TABLE",LF
            .asc "BROUGHT UP SO i CAN",LF,"FINISH THIS OTHER",LF
            .asc "PROJECT.'",LF,LF
            .asc "jEFFERSON HANDS",LF,"YOU HIS DESK.",EOL,EOL
aNeedTix:   .asc "fRECKLE-FACED KID AT",LF,"THE COUNTER STOPS",LF
            .asc "YOU. 'yOU NEED A",LF,"TICKET TO GET IN!'",EOL
            .asc "'yOU BETTER GET IN,",LF,"bABE'S ON DECK!'",EOL
aBuyTix:    .asc "'tHAT'S SOME FUNNY",LF,"LOOKIN' MONEY, BUT i",LF
            .asc "GUESS IT SPENDS THE",LF,"SAME. hERE'S YOUR",LF
            .asc "TICKET.'",EOL,"tHE KID SAYS YOU",LF
            .asc "NEED 'mONEY...?'",EOL
aBallHit:   .asc "yOU HEAR THE CRACK",LF,"OFF rUTH'S BAT AND",LF
            .asc "SEE THE BALL HEADING",LF,"RIGHT AT YOU. yOU",LF
            .asc "KNEW EXACTLY WHERE TO",LF,"BE...",EOL
            .asc "tHE GAME GOES ON.",EOL
aMissed:    .asc "tHAT MOMENT'S PASSED.",LF,"lET IT GO",EOL,EOL
aTryCatch:  .asc "wITH A SATISFYING",LF,"THUD, THE BALL PLANTS",LF
            .asc "ITSELF IN THE",LF,"PILFERED GLOVE. fANS",LF
            .asc "EYE YOU SUSPICIOUSLY.",LF,LF,"yOU... BETTER GET OUT",LF
            .asc "OF HERE.",EOL
            .asc "wITH NOTHING TO CATCH",LF,"THE BALL, IT BOUNCES",LF
            .asc "OUT OF YOUR HANDS AND",LF,"INTO THE HANDS OF",LF
            .asc "ANOTHER FAN, WHO RUNS",LF,"LAUGHING FROM nAVIN",LF
            .asc "fIELD.",EOL
aToJail:    .asc "a LITTLE KID CRIES",LF,"AND POINTS AT YOU,",LF
            .asc "'sTOLE MY GLOVE!'",LF,LF,"cROWD DISAPPROVES,",LF
            .asc "AND SO DO dETROIT'S",LF,"fINEST...",EOL,EOL
aOpSarc:    .asc "dID YOU MISS THE BIT",LF,"ABOUT THE GRANITE?",LF
            .asc "tHE LID ALONE WEIGHS",LF,"SEVERAL THOUSAND KG,",LF
            .asc "AND IT'S NOT WHAT",LF,"YOU'RE HERE FOR.",EOL,EOL
