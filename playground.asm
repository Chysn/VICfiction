;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                                  Marooned
;                     Demo of VICfiction for Unexpanded VIC
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
            
; Configuration
TIME_LIMIT  = 0                 ; Time limit (0=No limit)
HOUR_OFFSET = 5                 ; Hour offset
SCRCOL      = 8                 ; Screen color
COL_INPUT   = 5                 ; Input color
COL_NORM    = 30                ; Text color
COL_ALERT   = 28                ; Alert color
COL_ITEM    = 158               ; Item color
COL_ROOM    = 31                ; Room name color

; Game Memory Locations
VERB_ID     = $00               ; Verb ID
ITEM_ID     = $01               ; Item ID
PATTERN     = $02               ; Pattern (2 bytes) 
INVENTORY   = $a4               ; Inventory (2 bytes) 
CURR_ROOM   = $a6               ; Current room
END_ITEM    = $a7               ; Index of end of item list
ACT_RESULT  = $a8               ; At least one action had a result
TEMP        = $a9               ; Temporary value (3 bytes)
FROM_ID     = $a9               ; From ID during transform
TO_ID       = $aa               ; To ID during transform
TIMER       = $ac               ; Game timer
RM          = $ad               ; Room address (2 bytes)
SCORE       = $af               ; Score
BUFFER      = $0220             ; Input buffer
SEEN_ROOMS  = $0340             ; Marked as 1 when entered
ITEM_ROOMS  = $0360             ; RAM storage for item rooms

; Operating System Memory Locations
CASECT      = $0291             ; Disable Commodore case
VIC         = $9000             ; VIC chip offset
VIA1PA1     = $9111             ; VIA NMI Reset
ISCNTC      = $ffe1             ; Check Stop key
COL         = $d3               ; Screen column

; NMI Restore
NMINV       = $0318             ; Release NMI vector
;-NMINV     = $fffe             ; Development NMI non-vector (uncomment for dev)

; Routines
PRINT       = $cb1e             ; Temporary print
CHRIN       = $ffcf             ; Get input
CHROUT      = $ffd2             ; Character out
PRTFIX      = $ddcd             ; Decimal display routine
  
; Constants
SPACE       = $20               ; Space
EOL         = $00               ; End of line or list
LF          = $0d               ; Linefeed
RVS_ON      = $12               ; Reverse on
RVS_OFF     = $92               ; Reverse off
CRSR_UP     = $91               ; Cursor Up
GO_CMD      = 1                 ; Basic Command - GO
LOOK_CMD    = 2                 ;               - LOOK
GET_CMD     = 3                 ;               - GET
DROP_CMD    = 4                 ;               - DROP
INV_CMD     = 5                 ;               - INVENTORY
IS_INVIS    = $01               ; Item Property - Invisible
IS_UNMOVE   = $02               ;               - Unmoveable
IS_PLHOLDER = $04               ;               - Placeholder
IS_TIMER    = $08               ;               - Timer Display

; Initialize 
Init:       lda #<NewGame       ; Install the custom NMI (restart)
            sta NMINV           ; ,, 
            lda #>NewGame       ; ,,
            sta NMINV+1         ; ,, 
            lda #SCRCOL         ; Set screen color
            sta VIC+$0f         ; ,,
            lda #242            ; Set upper/lower charset
            sta VIC+$05         ; ,,
            lda #$80            ; Disable Commodore-Shift
            sta CASECT          ; ,,
            ; Fall through to New Game

; New Game
NewGame:    bit VIA1PA1         ; Reset NMI
            jsr ISCNTC          ; If STOP is down as the game starts
            bne start           ;   we'll go to the READY screen
            brk                 ;   ,,
start:      ldx #0              ; 
            stx INVENTORY       ; Clear Inventory
            stx INVENTORY+1     ; ,,
            stx TIMER           ; Reset timer
-loop:      inx                 ; Copy initial room location data to     
            lda ItemRoom-1,x    ;   RAM, so it can be updated as items are
            sta ITEM_ROOMS-1,x  ;   moved around
            lda Item1-1,x       ; Check for last item
            bne loop            ;   If not the last item, keep going
            stx END_ITEM        ;   store end item for use as a delimiter
            ldx #$1f            ; Clear the Seen Rooms list, which is used
-loop:      sta SEEN_ROOMS,x    ;   to show room details on first entry, then
            dex                 ;   suppress for subsequent entries
            bne loop            ;   ,,
            cli                 ; Clear SEI from ROM hardware vector
            lda #<Intro         ; Show intro
            ldy #>Intro         ; ,,
            jsr PrintMsg        ; ,,            
            ldx #1              ; Initialize starting room id
            stx CURR_ROOM       ; ,,
            jmp ShowRmN         ; Show room name before game starts
                 
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
Process:    clc                 ; Clear the action result flag
            ror ACT_RESULT      ; ,,
            ldx #$ff            ; Look for actions for this command's verb
next_act:   inx                 ; ,,
            lda ActVerb,x       ; ,,
            bne have_verb       ; After game-specific actions, do built-in
            jmp PostAction      ; Game-specific actions are done
have_verb:  cmp VERB_ID         ; ,,
            bne next_act        ; ,,
found_verb: lda ActItem,x       ; Get the item for this action
            beq item_ok         ;   If the action specifies no item, skip
            cmp ITEM_ID         ; Does the item match the action's item?
            bne next_act        ;   If not, go back for next action            
item_ok:    jsr ItemInRm        ; Is the item in the room?
            bcs ch_cond
            jmp NotHere
ch_cond:    lda ActInvCon,x     ; Is there an inventory item condition?
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
            ror ACT_RESULT      ; Set the action result flag
            lda ActResTxtH,x    ; Show the failure message for the action
            beq next_act        ;   ,, (If high byte=0, it's a silent failure)
            tay                 ;   ,,
            lda ActResTxtL,x    ;   ,,
            jsr PrintAlt        ;   ,,
            jmp next_act        ;   ,,
success:    sec                 ; SUCCESS!
            ror ACT_RESULT      ; Set the action success flag
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
move_pl:    sta CURR_ROOM       ; Set current room specified by To ID
            jmp next_act        ; Then continue processing actions
is_from:    sta FROM_ID         ; Store the From ID temporarily
            lda ActTo,x         ; A = To ID?
            bne xform           ;   If so, do the transform
            ldy FROM_ID         ; If To=0 then move the item in From ID to
            lda CURR_ROOM       ;   the current room
            sta ITEM_ROOMS-1,y  ;   ,,
            jmp next_act        ; Contine processing
xform:      sta TO_ID           ; Transform - Put To where From is
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
            bne next_act2       ;   then get rid of it
            lda #0              ;   ,,
            sta INVENTORY+1     ;   ,,
            jmp next_act        ;   Check next action
ch_inv2:    lda INVENTORY+1     ; Is the From item in the right hand?
            cmp FROM_ID         ;   ,,
            bne next_act2       ;   If not, check next action
            lda TO_ID           ;   If so, switch
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
PostAction: bit ACT_RESULT      ; Bypass the basic actions if one or more
            bmi h_timer         ;   database actions was attempted
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
no_cmd:     jmp Nonsense        ; Error message if no match
h_timer:    jsr AdvTimer        ; Advance timer
post_r:     jmp Main            ; Return to Main main routine

; Do Drop
; Of item ID
DoDrop:     ldy #1              ; Y is the hand index
ch_hand:    lda INVENTORY,y     ; Is the specified item in this hand?
            cmp ITEM_ID         ;   ,,
            beq drop_now        ;   If so, drop it
            dey                 ;   If not, check the other hand
            bpl ch_hand         ;   ,,
            lda #<NoDropTx      ; The item was not found in inventory, so
            ldy #>NoDropTx      ;   show the "don't have it" message
            jsr PrintRet        ;   ,,
drop_now:   tax                 ; X is the item index
            lda CURR_ROOM       ; Put the item in the current room
            sta ITEM_ROOMS-1,x  ; ,,
            lda #0              ; Remove it from the hand it's in
            sta INVENTORY,y     ; ,,
            jsr Confirm         ; Show the confirmation message
            jmp Main            ; Return to Main routine

; Do Look                        
DoLook:     lda ITEM_ID         ; Get the current item id
            beq ShortRoom       ; If there's an item id, then interpret LOOK
has_item:   jmp look_item       ;   as a request to look at that item
ShortRoom:  jsr RoomName
            jsr PrintAlt
            ; Directional display
show_dir:   jsr SetRmAddr       ; Get room address to (RM)
            ldy #5              ; 5 is room index for North parameter
-loop:      lda (RM),y          ; Get room id for room in this direction
            beq next_dir        ; Is there a room that way?
            lda #RVS_ON         ; Reverse on
            jsr CHROUT           ; ,,
            lda Directions,y    ; If so, get the character
            ora #$80            ;   make it uppercase
            jsr CHROUT          ;   and print it
            lda #RVS_OFF        ; Reverse off
            jsr CHROUT          ; ,,
            lda #' '
            jsr CHROUT          
next_dir:   dey                 ; Get next direction
            bpl loop            ;   until done            
dir_end:    jsr Linefeed        ; 2 x Linefeed  
            jsr Linefeed        ; ,,  
            ; Item display
            lda #COL_ITEM       ; Set item color
            jsr CHROUT          ; ,,
            ldx #0              ; X is the item index
next_item:  lda CURR_ROOM
            inx                 ; ,,
            cpx END_ITEM        ; Has the last item been reached?
            beq look_r          ;   If so, move back to input main routine
            cmp ITEM_ROOMS-1,x  ; Is the indexed item in the current room?
            bne next_item       ;   If not, check next item
            lda ItemProp-1,x    ; The item is in the current room, but is it
            and #IS_INVIS       ;   visible?
            bne next_item       ;   If not, check the next item
            lda ItemTxtL-1,x    ; ,,
            ldy ItemTxtH-1,x    ; ,,
            jsr PrintNoLF       ; ,,
            jmp next_item       ; Go to the next item
            ; Look at a single item
look_item:  jsr ItemInRm        ; The LOOK command took an item id. Is that item
            bcs in_room         ;   in the current room?
NotHere:    lda #<NotHereTx     ; If the specified item isn't in the room or
            ldy #>NotHereTx     ;   inventory, show a message and go back
            jmp PrintRet        ;   for another command
in_room:    tax                 ; X = specified item id
            lda ItemTxtH-1,x    ; Show the description of the item
            tay                 ; ,,
            lda ItemTxtL-1,x    ; ,,
            jsr PrintAlt        ; ,,
            lda ItemProp-1,x
            and #IS_TIMER
            beq look_r
            ldx #HOUR_OFFSET
            lda TIMER
sub_hour:   cmp #59
            bcc lt_hour
            sec
            sbc #60
            inx
            bne sub_hour
lt_hour:    pha
            lda #0
            jsr PRTFIX
            lda #':'
            jsr CHROUT
            pla
            cmp #10
            bcs show_min
            pha
            lda #'0'
            jsr CHROUT
            pla
show_min:   tax
            lda #0
            jsr PRTFIX
            jsr Linefeed
look_r:     jmp Main            ; Return to Main routine

; Do Go           
DoGo:       ldx #0              ; Starting from the first character,
            jsr GetPattern      ;   skip the first pattern (GO/MOVE)
            jsr GetPattern      ;   and get the second
ShortGo     jsr SetRmAddr       ; Get room address to (RM)
            ldy #5              ; 5 is room index for North parameter
-loop:      lda Directions,y    ; A is character for direction
            cmp PATTERN         ; Is this the attempted direction?
            beq try_move        ;   If so, try moving that way
            dey                 ; Get next direction
            bpl loop            ;   until done
            bmi invalid         ; Direction name not found
try_move:   lda (RM),y          ; Get the room id at the found index
            beq go_fail         ; Player is going an invalid direction
            sta CURR_ROOM       ; But if direction is valid, set room
            jsr AdvTimer        ; Advance Timer on move
ShowRmN:    lda #COL_ROOM       ; Always show room name after move
            jsr CHROUT          ; ,,
            jsr Linefeed        ; ,,
            jsr RoomName        ; ,,
            jsr PrintMsg        ; ,,
            ldx CURR_ROOM       ; Is this the first time this room has been
            lda SEEN_ROOMS-1,x  ;   visited?
            bne go_r            ; Already been visitied, so show only name
            lda #1              ; Otherwise, flag the room as seen, and
            sta SEEN_ROOMS-1,x  ;   show everything else (description, items,
            jsr NormCol         ;   directional display)
            jmp ShortRoom       ;   ,,
invalid:    jmp ShowErr         ; Like Nonsense, but don't look at shortcuts
go_fail:    lda #<NoPathTx      ; Show "no path" message and return to Main
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
            lda ItemProp-1,y    ; If the item is a placeholder, it will not
            and #IS_PLHOLDER    ;   be used as an Item ID in a command
            bne loop            ;   ,,
            sty ITEM_ID         ; Item has been found. Set ItemID and return
itemid_r:   rts                 ; ,,

; Set Room Address
; Find current room's starting data address and store it in (RM)
SetRmAddr:  lda #<Rooms         ; Set starting room address
            sta RM              ; ,,
            lda #>Rooms         ; ,,
            sta RM+1            ; ,,
            ldx CURR_ROOM       ; X = room id
            dex                 ; zero-index it
            beq setaddr_r       ; if first room, (RM) is already set
-loop:      lda #8              ; Add 8 for each id
            clc                 ; ,,
            adc RM              ; ,,
            sta RM              ; ,,
            bcc nc1             ; ,,
            inc RM+1            ; ,,
nc1:        dex                 ; ,,
            bne loop            ; Multiply
setaddr_r:  rts

; Set Room Name
; Set up A and Y for room description display. This should be followed by
; PrintAlt (for name), or PrintMsg (for description)         
RoomName:   jsr SetRmAddr       ; Get room address to (RM)
            ldy #6              ; 6 = low byte parameter
            lda (RM),y          ; A is low byte
            pha                 ; Push low byte to stack
            iny                 ; 7 = high byte parameter
            lda (RM),y          ; A is high byte
            tay                 ; Y is now high byte (for PrintMsg)
            pla                 ; A is now low byte (for PrintMsg)
            rts

; Advance Timer            
AdvTimer:   inc TIMER           ; Increment game timer
            beq timer_r         ; If it's zero, ignore the time
            lda TIMER           ; Did the timer match the deadline?
            cmp #TIME_LIMIT     ; ,,
            bne timer_r         ; If not, do nothing         
            lda #<TimeTx        ; Show the time-out message
            ldy #>TimeTx        ; ,,
            jsr PrintMsg        ; ,,
            pla                 ; This is from a JSR, so clear the return
            pla                 ;   address off the stack
            jmp game_over       ; Game Over
timer_r:    rts
            
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
            jsr Linefeed
            ldx TEMP+2
            rts

; Normal Color Shortcut
NormCol:    lda #COL_NORM
            .byte $3c

; Linefeed Shortcut
Linefeed:   lda #LF
            jmp CHROUT 
               
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TEST GAME DATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
Directions: .asc 'DUWESN'

; Text - Game Messages, Errors, etc.
Intro:      .asc 147,COL_NORM,"tHE SUN BEATS DOWN",LF,"ON YOU. yOU DON'T",LF
            .asc "REMEMBER MUCH, BUT",LF,"CLEARLY YOU MADE",LF
            .asc "YOUR CREW PRETTY MAD.",LF,LF,
            .asc 156,"mAROONED",LF,LF,"pROOF-oF-cONCEPT",LF
            .asc "FOR vicFICTION eNGINE",EOL
NoVerbTx:   .asc COL_ALERT,"nO NEED TO DO THAT",EOL
NoDropTx:   .asc COL_ALERT,"yOU DON'T HAVE THAT",EOL
HaveItTx:   .asc COL_ALERT,"yOU ALREADY HAVE IT",EOL
GameOverTx: .asc COL_ALERT,"gAME OVER!",EOL
NotHereTx:  .asc COL_ALERT,"tHAT'S NOT HERE",EOL
NoPathTx:   .asc COL_ALERT,"yOU CAN'T GO THAT WAY",EOL
NoMoveTx:   .asc COL_ALERT,"iT CAN'T BE MOVED",EOL
FullTx:     .asc COL_ALERT,"bOTH HANDS ARE FULL",EOL
ConfirmTx:  .asc COL_ALERT,"ok",EOL
TimeTx:     .asc COL_ALERT,"oUT OF TIME!",EOL

; Verbs
;   VerbID - Cross-referenced ID list for verb synonyms
; Basic - GO (MovE), LooK (L), GeT (TakE), DroP, InventorY (I)
; Extended - swim,dig,drink,wish,raise(pull),open(unlock)
Verb1:      .byte "G","M","L","L","G","T","D","I","I"     ; Basic Verbs
VerbL:      .byte "O","E","K","L","T","E","P","Y","I"     ; Basic Verbs
VerbID:     .byte 1,1,2,2,3,3,4,5,5                       ; Basic Verbs

; Rooms
;                 D U W E S N Desc. Low Addr, Desc. High Addr
Rooms:      .byte 0,2,0,3,0,0,<RForest,>RForest
            .byte 1,0,0,0,0,0,<RTree,>RTree
            .byte 0,0,1,3,0,0,<RRiverside,>RRiverside
            .byte 0,0,0,0,0,3,<RCave,>RCave

; Room Descriptions
;     The room name is terminated by EOL, after which is the room description,
;     also terminated by EOL
RForest:    .asc "fOREST",EOL,"tHERE ARE TREES",LF,"EVERYWHERE",EOL
RTree:      .asc "tREE",EOL,"yOU SEE EVERYTHING",LF,"FROM UP HERE!",EOL
RRiverside: .asc "rIVERSIDE",EOL,"tHERE'S A cave",LF,"ENTRANCE",EOL
RCave:      .asc "cAVE",EOL,"tOO DARK TO SEE!",EOL

; Items
;   Item1    - First Character
;   ItemL    - Last Character
;   ItemRoom - Starting Room ID
;   ItemProp - Item Properties
;     Bit 0 = Is invisible (not shown in room, but can be interacted with)
;     Bit 1 = Is un-moveable (cannot move from its room)
;     Bit 2 = Is placeholder (cannot be used as an item)
;     Bit 3 = Is timekeeping device (shows number of action attempts) 
;   ItemTxt  - Address of item name and description
;   (The item name is terminated by EOL, after which is the item description,
;    also terminated by EOL)
; 
Item1:      .byte 'W','C','S','C',EOL
ItemL:      .byte 'H','E','D','K',EOL
ItemRoom:   .byte  1 , 3,  1,  2, EOL
ItemProp:   .byte  8 , 3,  0,  0, EOL
ItemTxtL:   .byte <IWatch,<ICave,<ISword,<ICloak,EOL
ItemTxtH:   .byte >IWatch,>ICave,>ISword,>ICloak,EOL

; Item Descriptions
IWatch:     .asc "cHEAP watch",EOL,"tHE MINUTE HAND SAYS",EOL
ICave:      .asc "cave",EOL,"a SMALL CAVE ENTRANCE",EOL
ISword:     .asc "sWORD",EOL,"sHARP AND STABBY",EOL
ICloak:     .asc "iNVISIBLE cLOAK",EOL,"yOU CANNOT SEE IT",EOL

; Actions
;   ActVerb    - The Verb ID for this action
;   ActItem    - The Item ID for this action. If 0, no item is used.
;   ActInvCon  - The player must be holding this Item ID for success in this
;                action. If 0, no item needs to be held.
;   ActRoomCon - The Item ID must be in this room for success in this action.
;                If 0, the action is not conditioned on this item. If both 
;                ActHoldCon and ActRoomCon are non-0, then both conditions must
;                be met for the action to be successful. Note that this item may
;                be in inventory.
;   ActInvExlc - The player must NOT be holding this Item ID for success in this
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
;   ActResTxt  - The address of the success and failure messasges
;                (The success message is terminated by EOL, after which is the
;                 failure message, also terminated by EOL)
ActVerb:    .byte GO_CMD,EOL
ActItem:    .byte 2,EOL
ActInvCon:  .byte 0,EOL
ActRoomCon: .byte 0,EOL
ActInvExcl: .byte 3,EOL
ActFrom:    .byte 0,EOL
ActTo:      .byte 4,EOL
ActResTxtL: .byte <ACave
ActResTxtH: .byte >ACave

; Action Results
ACave:      .asc "yOU ENTER THE CAVE",EOL,"nOT WITH THAT SWORD!",EOL
