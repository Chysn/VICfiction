;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                               VICfiction Engine
;
;                            (c) 2022, Jason Justian
;                  
; Assembled with XA
;
; xa -O PETSCII -M -o story_name.bin 
;    ./src/VICfiction.asm ./src/StoryName.story.asm
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
; SCRIPTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Command scripting, for regression testing, can be performed using a serial
; text file, with commands separated by $0d, and then $0d at the end.
; Scripts can be run from BASIC like this
;
; OPEN2,8,2,"SCRIPT,S,R":POKE781,2:SYS65478:SYS8192
;
; When the script has completed, control will be turned back over to the
; keyboard.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
* = $2000                       ; Block 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; VICFICTION SETTINGS AND LABELS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;            
; Engine Memory Locations
VERB_ID     = $00               ; Verb ID
ITEM_ID     = $01               ; Item ID
PATTERN     = $02               ; Pattern (2 bytes) 
ACT_SUCCESS = $a6               ; At least one action was successful
ACT_FAILURE = $a7               ; At least one action failed
TEMP        = $a9               ; Temporary values (4 bytes)
FROM_ID     = $ad               ; From ID during transform
TO_ID       = $ae               ; To ID during transform
SCORE       = $af               ; Score (number of scored items in SCORE_RM)
DELIM       = $b1               ; String delimiter
COL         = $b2               ; Column number
PRPTR       = $b3               ; Printer pointer
RM          = $fb               ; Room address (2 bytes)
DIR         = $fd               ; Direction name pointer (2 bytes)
BUFFER      = $0200             ; Input buffer

; Saved Memory Locations
GAME_DATA   = $1d00             ; Entirety of game data (256 bytes)
SEEN_ROOM   = $1d00             ; Marked as 1 when entered (80 bytes)
TIMER       = $1d50             ; Timer countdown values (48 bytes)
ITEM_ROOM   = $1d80             ; Room ID for item, or $80 for inv (126 bytes)
GAMEOVER    = $1dfe             ; Flag if game is over
CURR_ROOM   = $1dff             ; Current Room ID

; Operating System Memory Locations
CASECT      = $0291             ; Disable Commodore case
VIC         = $9000             ; VIC chip offset
KBSIZE      = $c6               ; Keyboard buffer size
STATUS      = $90               ; File status
OPENFILES   = $98               ; Open logical files
KEYCVTRS    = $028d             ; Keyboard codes
COLNUM      = $d3               ; KERNAL column number

; NMI
NMI         = $ff5b             ; Do nothing (RTI)
NMINV       = $0318             ; Release NMI vector
;-NMINV     = $fffe             ; Development NMI non-vector (uncomment for dev)

; Routines
PRINT       = $cb1e             ; Temporary print
CHRIN       = $ffcf             ; Get input
CHROUT      = $ffd2             ; Character out
PRTFIX      = $ddcd             ; Decimal display routine
SETLFS      = $ffba             ; Setup logical file
SETNAM      = $ffbd             ; Setup file name
SAVE        = $ffd8             ; Save
LOAD        = $ffd5             ; Load
CLALL       = $ffe7             ; Clear file table
  
; Constants
SPACE       = $20               ; Space
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
WAIT_CMD    = 6                 ;               - WAIT
IS_INVIS    = $01               ; Item Property - Invisible
IS_UNMOVE   = $02               ;               - Unmoveable
IS_PLHOLDER = $04               ;               - Placeholder
IS_CLOCK    = $08               ;               - Clock display
IS_FOLLOWER = $10               ;               - Follower
IS_NARRAT   = $20               ;               - Narrative item
IS_SCORED   = $40               ;               - Is scored
IS_LIGHT    = $80               ;               - Light source
IS_DARK     = $01               ; Room Property - Darkened
HIDE_DIR    = $02               ;               - Hide Directions
CLR_TIMERS  = $04               ;               - Stop timers (except 0)
ALT_DIR     = $08               ;               - Use alt directions

; Action Constants
EV          = $ff               ; Special action id for triggered action
ED          = $00               ; End of Data
TE          = $00               ; The End (Game Over) 

; RAM Start
; Used as jump to game when loaded from disk
; SYS8192
RAMStart:   jmp NewStory        ; BYPASS CARTRIDGE STUFF

; Initialize
; Used as target of autostart.asm in block 5
Init:       jsr $fd8d           ; Test RAM, initialize VIC chip
            jsr $fd52           ; Restore default I/O vectors
            jsr $fdf9           ; Initialize I/O registers
            jsr $e518           ; Initialize hardware
            lda #$19            ; Set string descriptor pointer, to avoid            
            sta $16             ;   conflicts with zero page addresses
            lda #$80            ; Turn control messages on for tape
            sta $9d             ; ,,
            lda #<NMI           ; Install the custom NMI handler, which does
            sta NMINV           ;   absolutely nothing.
            lda #>NMI           ;   ,,
            sta NMINV+1         ;   ,, 
            ; Fall through to New Story

; New Story
NewStory:   lda #SCRCOL         ; Set screen color
            sta VIC+$0f         ; ,,
            lda VIC+$05         ; Set lowercase character set
            ora #$02            ; ,,
            sta VIC+$05         ; ,,
            lda #$80            ; Disable Commodore-Shift
            sta CASECT          ; ,,          
            ldx #0              ; Initialize all 256 bytes of game data
            lda #0              ;   Seen rooms (80 bytes)
-loop:      sta GAME_DATA,x     ;   Timers (48 bytes)
            dex                 ;   Score, game over status
            bne loop            ;   ,,
            stx KBSIZE          ; Clear keyboard buffer
-loop:      inx                 ; Copy initial room location data to     
            lda ItemRoom-1,x    ;   RAM, so it can be updated as items are
            sta ITEM_ROOM-1,x   ;   moved around
            lda Item1-1,x       ; Check for last item
            bne loop            ;   If not the last item, keep going
            ldx #$ff            ; Move Starting Inventory into inventory
-loop:      inx                 ;   ,,
            lda StartInv,x      ;   Get the inventory ItemID
            beq printro         ;   If it's the last one, exit
            tay                 ;   Y = inventory index
            lda #$80            ;   $80 here means "in inventory"
            sta ITEM_ROOM-1,y   ;   ,,
            bmi loop            ;   (unconditional because of lda #$80 above)
printro:    lda #147            ; Clear Screen
            jsr CHROUT          ; ,,
            jsr NormCol         ; Set normal color for intro text
            lda #<Intro         ; Show intro
            ldy #>Intro         ; ,,
            jsr PrintMsg        ; ,,
            lda #1              ; Initialize starting room
            sta CURR_ROOM       ;   ,, (Set room to 1 to avoid unwanted followers)
            jsr MoveTo          ;   ,,
            inc SEEN_ROOM       ;   and mark as seen
            jsr Linefeed        ; Show room name before game starts
            jmp EntryPt         ; ,,
                 
; Verb Not Found
; Show an error message, then go back for another command 
NoVerb:     lda BUFFER+1        ; Potentially process a shortcut if
            bne ShowErr         ;   only one character
            jmp ShortGo         ; Short form of GO (e.g., N,S,W,E,U,D)
ShowErr:    lda #<NoVerbTx      ; Show the error
            ldy #>NoVerbTx      ; ,,
            jsr PrintMsg        ; ,,
            ; Fall through to Main
 
; Main Routine      
Main:       lda #0              ; Clear the action success and failure
            sta ACT_SUCCESS     ;   flags
            sta ACT_FAILURE     ;   ,,
            lda #COL_INPUT      ; Set the color
            jsr CHROUT          ; ,,
            ldx #0              ; X is buffer index
            stx BUFFER          ; Clear the buffer
-loop:      jsr CHRIN           ; KERNAL CHRIN
            ldy OPENFILES       ; Support script testing. Is logical file open?
            beq ch_eol          ;   If not, work as normal with keyboard
            bit STATUS          ;   Is this end of file?
            bvc ch_eol          ;   ,,
            jsr CLALL           ;   ,, If end of file, close files
            jmp Main            ;   ,,
ch_eol:     cmp #LF             ; Did user press RETURN?
            beq enter           ;   If so, go to end of input
            and #$7f            ; Make case-insensitive
            sta BUFFER,x        ; Store in buffer at the index
            inx                 ; Next character
            cpx #88             ; Prevent buffer overflow
            bcc loop            ; ,,  
enter:      jsr CHROUT          ; Print the RETURN
            lda #0              ; Add the line-ending null            
            sta BUFFER,x        ; ,,
            lda OPENFILES       ; Support script testing. Is logical file open?
            beq no_echo         ;   If not, behave as normal
            lda #<BUFFER        ;   If so, echo the command
            ldy #>BUFFER        ;   ,,
            jsr PRINT           ;   ,,
            jsr Linefeed            
wait:       lda KEYCVTRS
            and #$01
            beq wait
no_echo:    lda BUFFER          ; Look at the first character for a
            beq Main            ;   couple things.
            cmp #'*'            ; System command
            bne ch_sp           ; ,,
            jmp System          ; ,,
ch_sp:      ldx #0              ; Discard leading spaces
            jsr GetPattern      ; ,,
            lda PATTERN         ; ,,
            beq Main            ; Drop to next line if no input
            lda GAMEOVER        ; If game is over, allow only system
            bne Main            ;   commands
            jsr Linefeed        ; Always a linefeed after a command
            ; Fall through to Transcribe

; Transcribe Text Buffer
;   to Verb ID and Item ID. The first pattern is already in PATTERN
;   from above.
Transcribe: jsr GetVerbID       ; Use the pattern to get Verb ID from database
            bcc NoVerb          ; Show an error if verb not found
            jsr GetPattern      ; Find the next two-character pattern
            jsr GetItemID       ; Use the pattern to get Item ID from database
            ; Fall through to StoryAct

; Story Action Lookup
; Start with the Action Engine. Look through the Story Actions for 
; matching commands and execute them.            
StoryAct:   ldx #$ff            ; Look for actions for this command's verb
next_act:   inx                 ; ,,
            lda ActVerb,x       ; ,,
            bne have_verb       ; After story actions
            jmp BasicAct        ;   do basic actions
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
item_ok:    jsr EvalAction      ; Evaluate the action         
            jmp next_act        ; Get the next action

; Do Event
; Prepare to evaluate a single action (id in X)           
DoEvent:    cpx #0              ; If no action is specified, do nothing
            bne do_event        ; ,,
            rts                 ; ,,
do_event:   lda #0              ; Clear success and failure flags for use
            sta ACT_SUCCESS     ;   by caller
            sta ACT_FAILURE     ;   ,,
            sta PATTERN         ; Clear PATTERN since events don't use it
            ; Fall through to EvalAction

; Evaluate Action
; X is the Action ID            
EvalAction: jsr NormCol         ; Messages will be normal color
            lda ActInvCon,x     ; Is there an inventory item condition?
            beq inv_ok          ;   If not, it's unconditional
            jsr ItemInInv       ; Is the player holding the specified item?
            bcc failure         ;   If not, the action fails
inv_ok:     lda ActRoomCon,x    ; Is there a room item condition?
            beq ch_excl         ;   If not, it's unconditional
            jsr ItemInRm        ; Is the item in the room or in inventory?
            bcc failure         ;   If no, the action fails
ch_excl:    lda ActInvExcl,x    ; Is there an item exclusion?
            beq success         ;   If not, the action is successful
            jsr ItemInInv       ;   If so, the action is successful unless
            bcc success         ;     the item is in inventory
failure:    sec                 ; FAILURE!
            ror ACT_FAILURE     ; Set the action failure flag
            bit ACT_SUCCESS     ; If a previous success message was shown,
            bmi eval_r1         ;   suppress the failure message
            lda ActResTxtH,x    ; Show the failure message for the action
            beq eval_r1         ;   ,, (If high byte=0, it's a silent failure)
            tay                 ;   ,,
            lda ActResTxtL,x    ;   ,,
            jsr PrintAlt        ;   ,,
eval_r1:    rts                 ; (nearby RTS to be in-bounds)
success:    ldy CURR_ROOM       ; SUCCESS! If a successful action happens,
            lda #1              ;   mark the room as "seen" to avoid duplicate
            sta SEEN_ROOM-1,y   ;   triggers
            lda ActResTxtH,x    ; Show the success message for the action
            beq ch_timer        ;   ,, (If high byte=0, it's a silent success)
            bit ACT_SUCCESS     ; If there has been a previous success for the
            bpl first_succ      ;   current command, print a linefeed for the
            pha                 ;   display of the second (and third, etc.)
            jsr Linefeed        ;   success.
            pla                 ;   ,,
first_succ: tay                 ; Y is the high byte of the success message
            lda ActResTxtL,x    ;   ,,
            jsr PrintMsg        ;   ,,
ch_timer:   lda ActTimer,x      ; Is a timer associated with this action?
            beq do_result       ; If not, perform the action result
            bmi set_timer       ; Bit 7 set means set the timer
            tay                 ; Bit 7 clear means clear the timer
            lda #0              ; ,,
            sta TIMER-1,y       ; ,,
            beq do_result       ; ,,
set_timer:  and #$7f            ; Mask away bit 7
            tay                 ; Y = Timer ID
            lda TIMER-1,y       ; Is the timer already set?
            bne do_result       ;   If so, do result
            lda TimerInit-1,y   ; Get timer init value
            sta TIMER-1,y       ; Initialize the timer
do_result:  sec                 ; Set the action success flag
            ror ACT_SUCCESS     ; ,,
            lda ActFrom,x       ; Now for the result. Get the From ID
            bne is_from         ;   Is there a From ID?
            bmi eval_r          ;   If high bit of FROM is set, message only
            lda ActTo,x         ; If there's no From ID, is there a To ID?
            bne move_pl         ;   If From=0 and To=ID then move player
game_over:  inc GAMEOVER        ; Flag game as over
            jsr Linefeed        ; Show score at the end, if applicable
            jsr CheckScore      ; ,,
            lda #<GameOverTx    ; If From=0 and To=0 then game over
            ldy #>GameOverTx    ;   Display the Game Over message...
            jsr PrintRet        ;   ,,
move_pl:    and #$7f            ; Mask away bit 7, if set
            jmp MoveTo          ; Set current room specified by To ID
is_from:    sta FROM_ID         ; Store the From ID temporarily
            lda ActTo,x         ; A = To ID?
            bne xform           ;   If so, do the transform
            ldy FROM_ID         ; If To=0 then move the item in From ID to
            lda CURR_ROOM       ;   the current room
            sta ITEM_ROOM-1,y   ;   ,,           
timer_done: rts                 ; Finish processing this action
            ; Transform the FROM item into the TO item
xform:      sta TO_ID
            cmp FROM_ID         ; If from and to are the same, no transform
            beq eval_r          ;   ,,
            bit TO_ID           ; If the specified TO ID has bit 7 set, then
            bpl ch_rooms        ;   it means "move the from item to this room"
            and #$7f            ;   ,, (mask away bit 7 to have Room ID)
            ldy FROM_ID         ;   ,,
            sta ITEM_ROOM-1,y   ;   ,, 
            rts                         
            ; Move TO item to where FROM item is and set FROM item to Nowhere
            ; if the FROM item was not found in inventory
ch_rooms:   ldy FROM_ID         ;   Get the From item's current location
            lda ITEM_ROOM-1,y   ;   ,,
            ldy TO_ID           ;   And store it into the To index
            sta ITEM_ROOM-1,y   ;   ,,
            ldy FROM_ID         ;   Take the From item off the board by
            lda #0              ;     setting its room to 0
            sta ITEM_ROOM-1,y   ;     ,,
eval_r:     rts

; Perform Basic Actions
; After the story action processing is complete      
; Built-In Actions include
;   - GO
;   - LOOK
;   - GET
;   - DROP   
;   - INVENTORY
BasicAct:   bit ACT_SUCCESS     ; Bypass the basic actions if one or more
            bmi h_timer         ;   story actions was successful
            lda VERB_ID         ; Get the entered verb
            cmp #GO_CMD         ; Handle GO/MOVE
            bne ch_look_c       ; ,,
            jmp DoGo            ; ,,
ch_look_c:  cmp #LOOK_CMD       ; Handle LOOK
            bne ch_get_c        ; ,,
            jmp DoLook          ; ,,
ch_get_c:   cmp #GET_CMD        ; Handle GET/TAKE
            bne ch_inv_c        ; ,,
            jmp DoGet           ; ,,
ch_inv_c:   cmp #INV_CMD        ; Handle INVENTORY
            bne ch_drop_c       ; ,,
            lda #$80            ; ,,
            jsr ShowInv         ; ,,
            jmp Main            ; ,,
ch_drop_c:  cmp #DROP_CMD       ; Handle DROP
            bne ch_z_c          ; ,,
            jmp DoDrop          ; ,,
ch_z_c:     cmp #WAIT_CMD       ; Handle WAIT
            bne no_cmd          ;   ,,
            jsr AdvTimers       ;   Only advance timers
            jmp Confirm         ;   and confirm
no_cmd:     bit ACT_FAILURE     ; If there was an action failure, then a failure
            bmi basic_r         ;   message was already shown
            jmp NoVerb          ; Error message if no match
h_timer:    jsr AdvTimers       ; Timer countdown or advance
basic_r:    jmp Main            ; Return to Main main routine

; Do Drop
; Of Item ID
DoDrop:     lda ITEM_ID         ; If no Item ID was found, show an
            bne ch_inv          ;   error message
            jmp not_inv         ;   ,,
ch_inv:     tax                 ; Y = Item ID
            lda ITEM_ROOM-1,x   ; Where is the item?
            bmi drop_now        ;   If $80, then in inventory, so drop
not_inv:    lda #<NoDropTx      ; The item was not found in inventory, so
            ldy #>NoDropTx      ;   show the "don't have it" message
            jsr PrintRet        ;   ,,
drop_now:   lda CURR_ROOM       ; Put the item in the current room
            sta ITEM_ROOM-1,x   ; ,,
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
sees_look:  lda PATTERN
            bne SingleItem
            jsr RoomDesc        ; Show room desciption
            jmp Main            
            ; Look at a single item
SingleItem: lda ITEM_ID         ; Get the Item ID
            beq Unknown         ; There was a pattern, but not an Item ID
            jsr ItemInRm        ; The LOOK command took an Item ID. Is that item
            bcs in_room         ;   in the current room?
NotHere:    lda #<NotHereTx     ; If the specified item isn't in the room or
            ldy #>NotHereTx     ;   inventory, show a message and go back
            jmp PrintRet        ;   for another command
Unknown:    lda #<UnknownTx     ; If the specified item doesn't have an Item ID,
            ldy #>UnknownTx     ;   show an Unknown Item message
            jmp PrintRet        ;   ,,
in_room:    tax                 ; X = specified Item ID
            jsr NormCol         ; Normal color description
            lda ItemTxtH-1,x    ; Show the description of the item
            tay                 ; ,,
            lda ItemTxtL-1,x    ; ,,
            jsr PrintAlt        ; ,,
            lda ItemProp-1,x    ; If this object is a timer, show its current
            and #IS_CLOCK       ;   value
            beq look_r          ;   ,,
            ldx TimerOffst      ; X is the number of "hours" in the display    
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
            lda PATTERN         ; If no direction pattern was provided,
            beq no_dir          ;   get clarification
ShortGo:    ldy #5              ; 5 is room index for North parameter
-loop:      lda (DIR),y         ; A is character for direction
            cmp PATTERN         ; Is this the attempted direction?
            beq try_move        ;   If so, try moving that way
            dey                 ; Get next direction
            bpl loop            ;   until done
            bmi invalid         ; Direction name not found
try_move:   lda (RM),y          ; Get the room id at the found index
            beq go_fail         ; Player is going an invalid direction
            jsr MoveTo          ; Set room address (RM) and CURR_ROOM            
EntryPt:    lda #COL_ROOM       ; Always show room name after move
            jsr CHROUT          ; ,,
            jsr RoomName        ; ,,
            jsr PrintMsg        ; ,,
            bit ACT_SUCCESS     ; If a room action was successful, don't
            bmi go_r            ;   show anything further
ch_first:   ldx CURR_ROOM       ; Is this the first time this room has been
            lda SEEN_ROOM-1,x   ;   visited?
            bne go_r            ; Already been visitied, so leave
            jsr Linefeed        ; Show room description
            jsr RoomDesc        ; ,,
            jmp Main         
invalid:    jmp ShowErr         ; Like NoVerb, but don't look at shortcuts
go_fail:    jsr IsLight         ; Failed to move. If there's light in the room
            bcs sees_path       ;   the player can see that they've moved
            jsr Linefeed        ; If it's dark, simply complain about it
            jmp ShowNoSee       ; ,,
no_dir:     lda #<NoDirTx       ; No direction was provided, so ask for one
            ldy #>NoDirTx       ; ,,
            jmp PrintRet        ; ,,
sees_path:  lda #<NoPathTx      ; Show "no path" message and return to Main
            ldy #>NoPathTx      ; ,,
PrintRet:   jsr PrintMsg        ; ,,
go_r:       jmp Main    

; Do Get            
DoGet:      lda PATTERN         ; Was an item provided?
            bne ch_known        ;   If not, show generic error
            jmp NoVerb          ;   ,,
ch_known:   lda ITEM_ID         ; Is there an item to get?
            bne ch_already      ;   If the item isn't recognized, show error
            jmp Unknown         ;   ,,
ch_already: jsr ItemInInv       ; Is the item in inventory already?
            bcc ch_in_rm        ; ,,
            lda #<HaveItTx      ; If in inventory, show the "already have it"            
            ldy #>HaveItTx      ;   message
            jmp PrintRet        ;   ,,
ch_in_rm:   tax                 ; X = Item ID
            jsr ItemInRm        ; If the item is not available to get, then
            bcc get_fail        ;   say it's not here
            lda ItemProp-1,x    ; Is this an un-moveable item?
            and #IS_UNMOVE      ; ,,
            bne unmoving        ; ,,
            ldx #0              ; Is there an empty inventory location?
            ldy #125            ; ,, Start with last item
-loop:      lda ITEM_ROOM-1,y   ; ,, Where is that item?
            bpl dn_count        ; ,, If it's not in inventory, don't count it
            inx                 ; ,, Otherwise, increment inventory count
dn_count:   dey                 ; ,, Iterate through all inventory
            bne loop            ; ,, ,,
            cpx #MAX_INV        ; Compare inv count to max inv
            bcs inv_full        ;   and notify if full
            ldy ITEM_ID         ; Y = item to pick up
            lda #$80            ; Mark the item as inventory
            sta ITEM_ROOM-1,y   ; ,,
Confirm:    lda #<ConfirmTx     ; Confirm the pick up
            ldy #>ConfirmTx     ; ,,
            jmp PrintRet        ; ,,
            ; Print various messages
get_fail:   jmp NotHere
unmoving:   lda #<NoMoveTx
            ldy #>NoMoveTx
            jmp PrintRet
inv_full:   lda #<FullTx
            ldy #>FullTx
            jmp PrintRet
     
; Show Room Items or Inventory
; Depending on entry point
; To show Inventory, call like
;     lda #$80
;     jsr ShowInv
ShowItems:  lda CURR_ROOM       ; Save room (or inventory marker) in TEMP
ShowInv:    sta TEMP            ;   against CHROUT
            lda #COL_ITEM       ; Set inventory color
            jsr CHROUT          ; ,,
            ldx #125            ; X is the Item X
-loop:      lda ITEM_ROOM-1,x   ; Where is this item?
            cmp TEMP            ; Is it where we're currently looking?
            bne next_item       ;   If no, increment and continue
            cmp #$80            ; If it's in inventory, show it always
            beq show_item       ; ,,
            lda ItemProp-1,x    ; The item is in the current room, but is it
            and #IS_INVIS       ;   visible?
            bne next_item       ;   If not, check the next item
show_item:  lda ItemTxtL-1,x    ; Show the text of the item
            ldy ItemTxtH-1,x    ;   ,,
            jsr PrintMsg        ;   ,,
next_item:  dex
            bne loop
            rts
                                                       
; Get Pattern
;   from the Input Buffer
;   starting at position X  
GetPattern: lda #0
            sta PATTERN
-loop:      lda BUFFER,x        ; Trim leading spaces by ignoring them
            beq pattern_r       ; ,,
            inx                 ; ,,
            cmp #' '            ; ,,
            beq loop            ; ,,
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
            cmp #ED             ;   Is this the end of the list?
            bne loop            ;   If not, try next verb
            clc                 ; Set error condition
            rts                 ; At end of list, return with carry clear
verb_found: lda VerbID-1,y      ; Cross-reference VerbID to handle synonyms
            tay                 ; ,,
            sty VERB_ID         ; Set VerbID, set carry, and return
            sec                 ; ,,
            rts                 ; ,,

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
            beq ch_ph           ; If end of list, return with ItemID at 0
            cmp PATTERN         ; Does the first character match?
            bne loop            ;   If not, try the next item
            lda ItemL-1,y       ; If so, does the second character match?
            cmp PATTERN+1       ;   ,,
            bne loop            ;   If not, try the next item
            sty ITEM_ID         ; Store Item ID provisionally
            tya                 ; Is the item with the found name in the
            jsr ItemInRm        ;   player's current room?
            bcc loop            ;   If not, continue
            sty ITEM_ID         ; Store the specific Item ID if found in room
ch_ph:      ldy ITEM_ID         ; Get the Item ID, if any 
            beq itemid_r        ; ,,
            lda ItemProp-1,y    ; If the item is a placeholder, it will not
            and #IS_PLHOLDER    ;   be used as an Item ID in a command
            beq itemid_r        ;   ,,
            ldy #0
            sty ITEM_ID         ; Item has been found. Set ItemID and return
itemid_r:   rts                 ; ,,
            
; Is Item In Room?
; Specify Item ID in A  
; Carry set if in room          
ItemInRm:   sta TEMP            ; Preserve A for caller
            sty TEMP+1          ; Preserve Y for caller
            tay                 ; Switch A to index
            lda ITEM_ROOM-1,y   ; Is the item in the current room?
            cmp CURR_ROOM       ; ,,
            beq yes_in_rm       ; ,, (Part of ItemInInv)
            cmp #$80            ; Is the item in inventory?
            beq yes_in_rm       ; ,,        
no_in_rm:   lda TEMP
            ldy TEMP+1
            clc                 ; Clear carry (not in room)
            rts

; Is Item In Inventory?
; Specify Item ID in A  
; Carry set if in inventory          
ItemInInv:  sta TEMP
            sty TEMP+1
            tay
            lda ITEM_ROOM-1,y
            bpl no_in_rm        ; (Part of ItemInRm)
yes_in_rm:  lda TEMP
            ldy TEMP+1
            sec                 ; Set carry (in room)
            rts
            
; Move To Room
; Set current room to A and add room parameter address to (RM)
MoveTo:     ldy CURR_ROOM       ; Stash the previous room for the followers
            sty TEMP            ; ,,
            sta CURR_ROOM       ; Set the new room
            ldy #0              ; Iterate through Item IDs
-loop:      iny                 ;   ,,
            lda Item1-1,y       ;   Go until end of items
            beq no_follow       ;     ,,
            lda ItemProp-1,y    ;   Is item a follower?
            and #IS_FOLLOWER    ;     ,,
            beq loop            ;     ,,
            lda TEMP            ;   If so, is it in the player's previous room?
            cmp ITEM_ROOM-1,y   ;     ,,
            bne loop            ;     ,,
            lda CURR_ROOM       ;   If it is, then move it to the player's new
            sta ITEM_ROOM-1,y   ;     room
            beq loop
no_follow:  txa
            pha
            jsr SetRoomAd       ; Set (RM) for room
            lda #ALT_DIR        ; Which direction set does this room use?
            jsr TestRmProp      ; ,,
            bne alt_dir         ; ,,
            lda #<Directions    ; ,,
            sta DIR             ; ,,
            lda #>Directions    ; ,,
            sta DIR+1           ; ,,
            jmp handle_tm       ; ,,
alt_dir:    lda #<AltDir        ; ,,
            sta DIR             ; ,,
            lda #>AltDir        ; ,,
            sta DIR+1           ; ,,
handle_tm:  ldx #0              ; Check Room Timers. X is the Timer ID
-loop:      inx                 ; Advance to next ID 
            lda TimerInit-1,x   ; Reached the end of Timers?
            beq ch_clear        ;   If so, exit
            lda CURR_ROOM       ; Get the current room
            cmp TimerRoom-1,x   ;   Is the timer in this room?
            bne loop            ;   If not, get next Timer            
            tay                 ; Y=CURR_ROOM. Should timer be initialized?
            lda TimerTrig-1,x   ;   ,,
            cmp #2              ;   If TimerTrig is 2, then init on any entry
            beq init_timer      ;     if timer is not started
            cmp #3              ;   If TimerTrig is 3, then init on any entry
            beq retrig          ;     even if timer is started
            cmp SEEN_ROOM-1,y   ;   If TimerTrig=0, the first time
            bne loop            ;   If TimerTrig=1, second and subsequent times
init_timer: lda TIMER-1,x       ; If the timer is already started, don't
            bne loop            ;   start it again
retrig:     lda TimerTest-1,x   ; Is there a test associated with this timer?
            beq timer_st        ; ,, If not, skip test evaluation
            stx TEMP+3          ; ,, Save X for this loop
            tax                 ; If so, put that Action ID into X
            jsr DoEvent         ;   and do the event
            ldx TEMP+3          ; Restore X for this loop
            bit ACT_SUCCESS     ; If the action was not successful, then
            bpl ch_clear        ;   skip timer start
timer_st:   lda TimerInit-1,x   ; START TIMER COUNTDOWN!
            sta TIMER-1,x       ; ,,
            bne loop            ; Go back for additional timers
ch_clear:   jsr AdvTimers       ; Do normal timer advance prior to clear check           
            lda #CLR_TIMERS     ; If the room being entered has the CLEAR TIMERS
            jsr TestRmProp      ;   property, then reset all timers to 0
            beq moveto_r        ;   except for the clock (timer 1).
            ldx #2              ;   ,,
-loop:      lda TimerInit-1,x   ;   ,, (No TimerInit value means end-of-timers
            beq moveto_r        ;   ,, ,,)
            lda #0              ;   ,,
            sta TIMER-1,x       ;   ,,
            inx                 ;   ,,
            bne loop            ;   ,,
moveto_r:   pla                 ; Restore X for caller
            tax                 ;   ,,
            rts
            
; Set Room Address
SetRoomAd:  lda #<Rooms         ; Set starting room address
            sta RM              ; ,,
            lda #>Rooms         ; ,,
            sta RM+1            ; ,,
            ldx CURR_ROOM       ; X = Room ID
            dex                 ; zero-index it
            beq set_rm_r        ; if first room, (RM) is already set
-loop:      lda #9              ; Add 9 for each id
            clc                 ; ,,
            adc RM              ; ,,
            sta RM              ; ,,
            bcc nc1             ; ,,
            inc RM+1            ; ,,
nc1:        dex                 ; ,,
            bne loop            ; Multiply
set_rm_r:   rts

; Set Room Name
; Set up A and Y for room description display. This should be followed by
; PrintMsg (for name), or PrintAlt (for description)         
RoomName:   jsr IsLight         ; Is the room illuminated?
            bcs sees_name       ; ,,
            jmp ShowNoSee       ; ,,
sees_name:  ldy #7              ; 7 = desc low byte parameter
            lda (RM),y          ; A is low byte
            pha                 ; Push low byte to stack
            iny                 ; 7 = high byte parameter
            lda (RM),y          ; A is high byte
            tay                 ; Y is now high byte (for PrintMsg)
            pla                 ; A is now low byte (for PrintMsg)
            rts
             
; Advance Timers    
; Advance (or coutdown) the Clock, then countdown each set Room Timer
; Timers are advanced when
;   1) The player moves to a new room
;   2) At least one story action was successful during a turn
;      (Cascaded actions on a single turn advance the timer once)     
AdvTimers:  lda TIMER           ; If the timer is 1, then it's not active
            beq adv_room_t      ;   so check other Timers
            clc                 ; Add to timer
            adc TimerDir        ;   $01 for +1, $ff for -1
            sta TIMER 
            cmp TimerTgt        ; Has clock hit the target?
            bne adv_room_t      ;   If not, check Room Timers
            ldx TimerAct        ; Do the specified timeout action
            jsr DoEvent         ; ,,
adv_room_t: ldx #1              ; X = Timer index (Timer 1 is the Clock, so
-loop:      inx                 ;   start with 2)
            lda TimerInit-1,x   ; Does the timer exist?
            beq timer_r         ;   If not, at end of timers, exit
            lda TIMER-1,x       ; Does the timer have a value?
            beq loop            ;   If not, get next timer
            dec TIMER-1,x       ; Decrement the timer
            bne loop            ; If it hasn't reached 0, get next timer
            txa                 ; Preserve X against event action
            pha                 ; ,,
            lda TimerAct-1,x    ; Get Action ID
            tax
            jsr DoEvent         ; Perform the event
            pla
            tax
            jmp loop            ; Get next timer
timer_r:    rts

; Room Description
; Show name, description, item list, directional display, and score
RoomDesc:   jsr IsLight         ; Is the room illuminated?
            bcs sees_desc       ; ,,
            jmp ShowNoSee       ; ,,
sees_desc:  jsr NormCol         ; Otherwise look refers to a whole room
            jsr RoomName        ; Get room information
            jsr PrintAlt        ; ,,
            ldx #125            ; Now show item descriptions in the room
-loop:      lda ITEM_ROOM-1,x   ; Where is this item?
            cmp CURR_ROOM       ; Is it in the current room?
            bne next_desc       ;   If no, increment and continue
            lda ItemProp-1,x    ;
            and #IS_NARRAT      ; Is it part of the room description?
            beq next_desc       ; ,,
            jsr Linefeed        ; Show the text of the item
            lda ItemTxtL-1,x    ;   ,,
            ldy ItemTxtH-1,x    ;   ,,
            jsr NextMsg         ; Get message after this one's ED
            jsr PrintAlt        ;   Print the optional third item message
next_desc:  dex
            bne loop
            lda #1              ; Mark the room as seen
            ldx CURR_ROOM       ; ,,
            sta SEEN_ROOM-1,x   ; ,,
            ; Fall through to ItemDisp

            ; Item display
ItemDisp:   jsr Linefeed
            jsr ShowItems
            
            ; Directional display
DirDisp:    lda #HIDE_DIR       ; Does this room hide directions?
            jsr TestRmProp      ; ,,
            bne ShowScore       ; ,, If so, skip right to score display
            lda #COL_DIR        ; Set directional display color
            jsr CHROUT          ; ,,
            lda #'('            ; Open paren
            jsr CHROUT          ; ,,
            ldy #5              ; 5 is room index for North parameter
-loop:      lda (RM),y          ; Get room id for room in this direction
            beq next_dir        ; Is there a room that way?
            lda (DIR),y         ; If so, get the character
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
ShowScore:  lda CURR_ROOM       ; Score is only shown in the score room
            cmp #SCORE_RM       ; ,,
            bne score_r         ; ,,
CheckScore: ldx #0              ; X is the item index
            stx SCORE           ; And reset score
-loop:      inx                 ; For each item
            lda Item1-1,x       ;   End of items?
            beq score_out       ;   ,,
            lda ItemProp-1,x    ;   Is it a scored item?
            and #IS_SCORED      ;   ,,
            beq loop            ;   ,,
            lda ITEM_ROOM-1,x   ;   Is it dropped in the current room?
            cmp #SCORE_RM       ;   ,,
            bne loop            ;   ,,  
            inc SCORE           ; Increment score if it qualifies  
            bne loop          
score_out:  ldx SCORE           ; If there's no score, don't display
            beq score_r         ; ,,
            jsr NormCol         ; Print the score test
            lda #<ScoreTx       ; ,,
            ldy #>ScoreTx       ; ,,
            jsr PRINT           ; ,,
            ldx SCORE           ; Use BASIC's PRTFIX to show the decimal score
            lda #0              ; ,, (X is low byte, A is high byte)
            jsr PRTFIX          ; ,,
            lda #'/'            ; Slash for "of"
            jsr CHROUT          ; ,,
            ldx #SCORE_TGT      ; And now show the target score
            lda #0              ; ,,
            jsr PRTFIX          ; ,,        
            jsr Linefeed        ; ,,
score_r:    rts
 
; Is Light
; Is there light in the current room?
; Carry set if yes
IsLight:    lda #IS_DARK        ; Test this room's IS DARK property
            jsr TestRmProp      ;   ,,
            beq room_light      ;   If not return with carry set           
            ldy #125            ; Look through inventory
-loop:      lda ITEM_ROOM-1,y   ;   ,,
            bmi ch_ls           ;   Found an inventory item. Is it a light?
nx_pl:      dey                 ;   ,,
            bne loop            ;   ,,
            clc                 ; Inventory searched, so exit with failure
            rts                 ;   (carry clear)
ch_ls:      lda ItemProp-1,y    ; Get item property
            and #IS_LIGHT       ;   Test for light source
            beq nx_pl           ;   If not, look for next potential light
room_light: sec                 ; A light source was found, so exit with success
            rts                 ;   (carry set)
            
; Test Room Property
; Specify property mask in A
;   BNE property_set
;   BEQ property_unset
TestRmProp: ldy #6              ; Get room property
            and (RM),y
            rts

; Find Next Message
; Given a Message address (A, Y), look for the ED+1 and return A, Y.            
NextMsg:    sta $07
            sty $08
            ldy #0
-loop:      lda ($07),y
            inc $07
            bne ch_end
            inc $08
ch_end:     cmp #0
            bne loop
set_print:  lda $07
            ldy $08       
            rts
            
; Print Alternate Message
; Given the Message address (A, Y), look for the ED+1, then print from there
PrintAlt:   jsr NextMsg
            ; Fall through to PrintMsg

; Print Message 1
; Print the first message at the specified address 
; 
; This print routine formats text for the VIC-20 screen. It's more complex
; than using the BASIC routine at $CB1E, but I considered it worth it, as it
; greatly improves the experience of authoring.
PrintMsg:   cpy #0              ; If the high byte is 0, just return
            beq p_r             ; ,,
            sta PRPTR           ; Set zero-page print pointer
            sty PRPTR+1         ; ,,
            txa                 ; Save X as iterator
            pha                 ; ,,
            ldy #0              ; Initialize string index
            ldx #0              ;   and word letter count
            sty COL             ;   and column pointer
            lda (PRPTR),y       ; If the very first character is 0, just
            bne next_ltr        ;   end this message without doing anything
            pla                 ;   ,,
            tax                 ;   ,,
p_r:        rts                 ;   ,,
next_ltr:   lda (PRPTR),y       ; Get character
            beq delimiter       ; End of string
            cmp #' '            ; End of word
            beq delimiter       ; ,,
            cmp #LF             ; End of line
            beq delimiter       ; ,,
            sta BUFFER,x        ; Store character
            inx                 ; Increment word character count
c_ltr:      inc COL             ; Increment column
            iny                 ; Increment string index
            bne next_ltr        ; Go back for next letter
delimiter:  stx TEMP+2          ; Store word length for compare
            sta DELIM           ; Stash the delimiter for later
            cpx #0              ; If there are no characters to print,
            beq lf1             ;   this is a single linefeed
            lda COL             ; For anything else, check for overflow
            cmp #22             ; ,,
            bcc prword          ; On overflow, drop to next line and
            stx COL             ;   Set length to the last word
            jsr Linefeed        ;   ,,
prword:     stx TEMP+2          ; Store word length for compare
            ldx #0              ; X is temporarily for print index
-loop:      lda BUFFER,x        ; Get letter from word buffer
            jsr CHROUT          ; Print that letter
            inx                 ; Increment print index
            cpx TEMP+2          ; Reached the word size?
            bne loop            ; ,, No, keep going
            lda COLNUM          ; If the column number won't accommodate
            cmp #21             ;   another character, skip the space
            bcs sk_space        ;   ,,
            lda #' '            ; Print a space after the word
            jsr CHROUT          ; ,,
sk_space:   ldx #0              ; Reset the word length for a new word
            lda DELIM           ; What was the delimiter?
            beq prmsg_r         ;   EOS - End of string
            cmp #LF             ;   LF - Linefeed
            bne c_ltr           ;   SPACE - Just go back for more
lf1:        lda #0              ; For a linefeed, reset the column to 0
            sta COL             ;   ,,
            jsr Linefeed        ;   and do a physical linefeed
            jmp c_ltr           ; Go back for more string
prmsg_r:    pla                 ; Restore X as iterator
            tax                 ; ,,
            jmp Linefeed
 
; Normal Color Shortcut
NormCol:    lda #COL_NORM
            .byte $3c

; Linefeed Shortcut
Linefeed:   lda #LF
            jmp CHROUT 

; System 
; Non-game commands, like Save and Load       
System:     lda BUFFER+1
            cmp #"s"
            beq Save
            cmp #"l"
            beq Load
            cmp #"q"
            beq quit
            cmp #"h"
            beq help
            jmp Main
quit:       jmp NewStory
help:       lda #<HelpTx
            ldy #>HelpTx
            jmp PrintRet

; Save
; To cassette, using the filename specified as SaveFile in the
; Story File
Save:       ldx #1              ; DEVICE=1 CASSETTE
            ldy #0              ; No command
            jsr SETLFS
            jsr SetName         ; SETNAM call
            ldx #<GAME_DATA     ; Start of data
            stx $c1             ; ,,
            lda #>GAME_DATA     ; ,,
            sta $c2             ; ,,
            ldy #>GAME_DATA     ; ,,
            iny                 ; End of data (+256 bytes)
            lda #$c1            ; Tab location
            jsr SAVE            ; Save
            jsr Linefeed
            jmp Main

; Load
; From cassette, using the filename specified as SaveFile in the
; Story File        
Load:       ldx #1              ; Tape device number
            ldy #1              ; Load to header location
            jsr SETLFS          ; ,,
            jsr SetName         ; SETNAM call
            lda #$00            ; Command for LOAD
            jsr LOAD            ; ,,
            bcc post_load       ; If LOAD completes, show room
            jsr Linefeed        ; ,,
            jmp Main            ; If any problem, just back to main
post_load:  jsr SetRoomAd       ; Set the (RM) pointer
            lda #COL_ROOM       ; Show room name after load
            jsr CHROUT          ; ,,
            jsr Linefeed        ; ,,
            jsr RoomName        ; ,,
            jsr PrintMsg        ; ,,       
            lda #0              ; Set Item ID and clear PATTERN, so it's
            sta ITEM_ID         ;   DoLook is treated as a look at the room
            sta PATTERN         ;   ,,
            jmp DoLook          ; Look at the room

; Set Name
; Shared between Save and Load            
SetName:    lda #EON-SaveFile   ; File name length
            ldx #<SaveFile      ; Set filename
            ldy #>SaveFile      ; ,,
            jmp SETNAM          ; ,,
