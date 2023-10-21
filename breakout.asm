; Utilizado o DASM para compilar
; veja em https://github.com/Julio-Rats/dasm
;
; Comando para compilar:
;   dasm breakout.asm -obreakout.bin -f3
;

    PROCESSOR 6502
    INCLUDE "vcs.h"
    INCLUDE "macro.h"


;===================================================================
;                     TV SYSTEM DEFINITION
;===================================================================
SYSTEM_TV = "NTSC"  ; (NTSC, PAL)

;===================================================================
;                       NTSC 60 FPS
    IF SYSTEM_TV == "NTSC"

KERNEL_SCANLINE     = 192
SCAN_START_BORDER   = 16
HEIGHT_LINES        = 8

VBLANK_TIMER        = 43
OVERSCAN_TIMER      = 38

BORDER_COLOR        = $04
PLAYER_COLOR        = $F4

LINE_COLOR1         = $46
LINE_COLOR2         = $36
LINE_COLOR3         = $26
LINE_COLOR4         = $16
LINE_COLOR5         = $C6
LINE_COLOR6         = $86

;===================================================================
;                       PAL 50 FPS
    ELSE 
        IF SYSTEM_TV == "PAL"

KERNEL_SCANLINE     = 228
SCAN_START_BORDER   = 24
HEIGHT_LINES        = 8

VBLANK_TIMER        = 52
OVERSCAN_TIMER      = 45

BORDER_COLOR        = $08
PLAYER_COLOR        = $44

LINE_COLOR1         = $62
LINE_COLOR2         = $64
LINE_COLOR3         = $46
LINE_COLOR4         = $26
LINE_COLOR5         = $56
LINE_COLOR6         = $D6

;===================================================================
;                          OTHERS
        ELSE
            ECHO "TV SYSTEM NOT SUPPORTED!"
        ENDIF
    ENDIF
;===================================================================

;===================================================================
;                       Global Constants
;===================================================================
PLAYER_LIFE         = 4
NUMBER_LINES        = 6 
HEIGHT_BORDER       = 16
HEIGHT_PLAYER       = 4
HEIGHT_BALL         = 3
SPEED_LEFT          = 3
SPEED_LEFT_HEX      = $30
SPEED_RIGHT         = 3
SPEED_RIGHT_HEX     = $D0
LAST_SCANLINE       = KERNEL_SCANLINE-3
SCAN_START_SCORE    = (SCAN_START_BORDER-10)-2
SCAN_POS_PLAYER     = (LAST_SCANLINE - HEIGHT_PLAYER)
SCAN_START_LINES    = (SCAN_START_BORDER + HEIGHT_BORDER + 21)

;===================================================================
;===================================================================
;           VARIABLES RAM ($0080-$00FF)(128B RAM)

    SEG.U   VARIABLES
    ORG     $80

COUNT_FRAMES    ds  1
SCORE_MASK      ds  1
COUNT_SCANLINES ds  1
COUNT_LIFE      ds  1
LINES_PFS0      ds  NUMBER_LINES
LINES_PFS1      ds  NUMBER_LINES
LINES_PFS2      ds  NUMBER_LINES
LINES_PFS3      ds  NUMBER_LINES
PLAYER_POS      ds  1
BALL_PHORZ      ds  1
BALL_PVERT      ds  1
BALL_STATUS     ds  1
;   BALL_STATUS DECODER
;      BIT         ACTION
;       0           Live?
;       1           Move up?
;       2           Move Right?
;      4-7          SPEED BALL
SCORE           ds  2
POINTER_SCORE   ds  8
POINTER_LIFE    ds  2

;===================================================================
;===================================================================
;                       CODE SEGMENT

    SEG   CODE
    ORG   $F000     ; Start of "Cart Area" (See Atari Memory Map)
;===================================================================
;                       CPU ENTRYPOINT
;===================================================================
BootGame:
    SEI
    CLD
    LDA #$00
    TAY
    LDX #$FF
    TXS
    INX

ClearMemory:
    DEX
    STA $CC,X
    BNE ClearMemory

    LDY #$08
    ; Set Position of P1 and M1
    STA WSYNC
    DEY
    ; Delay PosP1 and PosM1
PosPlayer1:
    DEY
    BPL PosPlayer1
    STA RESP1
    STA RESM1
    ; Set Pos P1 and M1(ball) (H,V) in Memory
    LDA #68
    STA PLAYER_POS
    LDA #75
    STA BALL_PHORZ
    LDA #KERNEL_SCANLINE
    STA BALL_PVERT
    LDA #$16
    STA BALL_STATUS
    ; Apply move if exists in buffer
    LDA #$20
    STA HMP1
    STA HMM1
    STA WSYNC
    STA WSYNC
    STA HMOVE
    STA WSYNC
    STA HMCLR

    ; Reset Game and Set Pointers
    JSR ResetGame
    JSR SetInfoDigitPointers

    LDA #%01000010  ; Starting Vblank
    STA VBLANK  

;===================================================================
;                         NEW FRAME CYCLE
;===================================================================
StartFrame:
    LDA #$02            ; Vertical sync is signaled by VSYNC's bit 1...
    STA VSYNC

    ; ...waiting 3 scanlines
    LDX #03
WsynWait:    
    DEX
    STA WSYNC           ; (WSYNC write => wait for end of scanline)
    BNE WsynWait

    STX VSYNC   ;X:=0   ; Signal vertical sync by clearing the bit (Start Vblank)

    LDA #VBLANK_TIMER   ; Timing Vblank Scanlines
    STA TIM64T
    
;===================================================================
;===================================================================
;                       Vblank code area
;===================================================================
;===================================================================
    ; Increment Count Frames
    INC COUNT_FRAMES
    
    ; Set Size and Graph type
    LDA #$30
    STA NUSIZ0
    LDA #$15
    STA NUSIZ1
    ; Control Mode Playfield to Score (No PlayField Reflect)
    LDA #0
    STA CTRLPF
    ; Input Controls Set Mode Read
    STA SWACNT 
    STA SWBCNT 
    ; Reset Vertical Delay
    STA VDELP0
    STA VDELP1
    STA VDELBL
    ; Set Color
    STA COLUBK
    LDA #BORDER_COLOR
    STA COLUP0
    LDA #PLAYER_COLOR
    STA COLUP1

    ; Set Position of P1 and M1
    LDY #$04
    ;Delay PosP0
    STA WSYNC
    DEY
PosPlayer0:
    DEY
    BPL PosPlayer0
    NOP
    STA RESP0
    ;Delay PosM0
    LDY #$05
PosMissile0:
    DEY
    BPL PosMissile0
    NOP
    NOP
    STA RESM0
    LDA #$F0
    STA HMP0

;===================================================================
;                  INPUT CONTROL PROCESSING AREA
;===================================================================
    ; Reset Switches
    LDA SWCHB
    AND #$01
    BNE P_Control
    JSR ResetGame
P_Control:
    ; Start Ball if ball is dead (button control)
    LDA BALL_STATUS
    AND #1
    BNE Controllers
    ; Get Input Button (Both Controls)
    LDA #$80
    AND INPT4
    BEQ Fire
    AND INPT5
    BEQ Fire
    JMP Controllers
    ; Ball is dead and button is push
Fire:
    ; Verify Count Life 
    LDA COUNT_LIFE
    BEQ Controllers
    ; "Use" life
    DEC COUNT_LIFE
    ; Ball Parameter Defaults
    LDA COUNT_FRAMES    
    AND #$04            ; Random Horz Start (Left or Right)
    ORA #$13
    STA BALL_STATUS
    ; Get input controls to move the player (platform)
Controllers:
    LDX SWCHA
    ; Control(s) press Left
    TXA
    AND #$40
    BEQ MovePlayerLeft
    TXA
    AND #$04
    BEQ MovePlayerLeft
    ; Control(s) press Right
    TXA
    AND #$80
    BEQ MovePlayerRight
    TXA
    AND #$08
    BEQ MovePlayerRight
    ; no press
    JMP NoMove
    ; Move Player to Left
MovePlayerLeft:
    ; Get current position, verify bounds, apply move based on movement of Player
    LDA PLAYER_POS
    CMP #26
    BCC NoMove
    LDX #SPEED_LEFT_HEX
    SEC
    SBC #SPEED_LEFT
    JMP SetMovemtMemory

MovePlayerRight: 
    ; Get current position, verify bounds, apply move based on movement of Player
    LDA PLAYER_POS
    CMP #111
    BCS NoMove
    LDX #SPEED_RIGHT_HEX
    CLC
    ADC #SPEED_RIGHT

SetMovemtMemory:        ; efficient ROM Space
    STA PLAYER_POS
    STX HMP1

NoMove:
    ; Ball Movement
    ; Check Dead Ball
    LDA BALL_STATUS
    TAY
    AND #1
    BEQ NoMoveBall
    ; Move
    ; Shift Speed Ball in 4 High Bits of BALL_STATUS
    LDX #4
    TYA
ShiftSpeedBall:
    LSR
    DEX
    BNE ShiftSpeedBall
    ; Save in Register X 
    TAX
    ; Move Vertical
    TYA
    AND #2
    BEQ MoveBallUP
; MoveBallDown:
    ; two's complement
    TXA
    EOR #$FF
    SEC
    ADC BALL_PVERT
    JMP MoveHorz

MoveBallUP:
    TXA
    CLC
    ADC BALL_PVERT
    
MoveHorz:    ; efficient ROM Space
    ; Save in Memory Vertical movement of Ball
    STA BALL_PVERT 
    ; Move Horizontal
    TYA
    AND #4
    BEQ MoveBallLeft
; MoveBallRight:
    TXA
    CLC
    ADC BALL_PHORZ
    STA BALL_PHORZ
    TYA
    AND #$F0
    EOR #$F0
    CLC
    ADC #$10
    JMP BallMoved
MoveBallLeft:
    TXA
    ; two's complement
    EOR #$FF
    SEC
    ADC BALL_PHORZ
    STA BALL_PHORZ
    TYA
    AND #$F0
BallMoved:
    STA HMM1
NoMoveBall:

; PreparePlayfield:     ; Preparing graph registers to start hot scanlines
    LDA #BORDER_COLOR
    STA COLUPF

    ; Wait Rest of Existing Vblank (Async Clock)
WaitVblankEnd:
    LDA INTIM
    BNE WaitVblankEnd

    ; Register Y for Count Hot Scanlines
    TAY ; A:=0
    STA WSYNC
    ; Apply Moves in Buffers
    STA HMOVE
    ; Clear Collisions (New Frame)
    STA CXCLR 
    STA WSYNC
    ; Clear Buffer of Moves
    STA HMCLR
    ; Out VBlank (Magic Starts Here)
    STA VBLANK          ; Stop Vblank

;=============================================================================================
;=============================================================================================
;=============================================================================================
;                                      KERNEL
;=============================================================================================
;=============================================================================================
;=============================================================================================

;                         PRINT SCREEN MOMENT (HOT SCANLINES)
;
;   Start Visible Scanlines
    ; Waiting for the Correct Scanline to Start Graphics
WaitPrintScore:
    ; Increment Y-ScanLine Count
    INY
    STA WSYNC
    CPY #(SCAN_START_SCORE-1)
    BCC WaitPrintScore
    ; Start Print Score and Life Counter
    STY COUNT_SCANLINES
    LDY #0
StartScore: 
    ;D0-1
    ; Get bitmap for the nº line of the Digit 0 (Most Significant Digit)
    LDA (POINTER_SCORE),Y
    ; Take the Left Mirror
    AND #$F0
    ; Save for Join
    STA SCORE_MASK
    ; Get Bitmap for the nº Line of the Digit 1
    LDA (POINTER_SCORE+2),Y
    STA WSYNC
    ; Take the Right Mirror
    AND #$0F
    ; Merge the Bitmaps to the Playfield 1 Register
    ORA SCORE_MASK
    STA PF1
    ;D2-3
    ; Get bitmap for the nº line of the Digit 2
    LDA (POINTER_SCORE+6),Y
    ; Take the Left Mirror
    AND #$F0
    ; Save for Join
    STA SCORE_MASK
    ; Get Bitmap for the nº Line of the Digit 3
    LDA (POINTER_SCORE+4),Y
    ; Take the Right Mirror
    AND #$0F
    ; Merge the Bitmaps to the Playfield 2 Register
    ORA SCORE_MASK
    STA PF2
    ; Get Bitmap for the nº Line of the Digit of Life Counter
    LDA (POINTER_LIFE),Y
    ; Take only the Right Mirror
    AND #$0F
    ; Set in Playfild tile of digit
    STA PF1
    LDA #0
    NOP
    INY
    STA PF2
    CPY #10     ; Length Bitmap (tile) of Digits (10 rows)
    BCC StartScore

; After Score Area
    ; Increment Y-ScanLine Count
    INY
    STA WSYNC
    STA PF1
    TYA
    CLC
    ; Adds to the Counter Lines How Many Lines Used in the Scoreboard
    ADC COUNT_SCANLINES
    STA COUNT_SCANLINES
    TAY
    ; Control Reflect Playfield for the Color Lines (No Reflection)
    LDA #$01
    STA CTRLPF   

    ; Wait to Print Borders
WaitPrintBord:
    ; Increment Y-ScanLine Count
    INY
    STA WSYNC
    CPY #(SCAN_START_BORDER-1)
    BCC WaitPrintBord

    ; Start Print Borders
    LDA #$FF
    LDX #$3F
    INY
    STA WSYNC
    ; Use Player to Make Left Border
    STA GRP0
    ; Not use the Left Part of the Playfild 1
    STX PF1
    STA PF2 
    
    ; Make UP border
StartBorder:
    ; Increment Y-ScanLine Count
    INY
    STA WSYNC
    CPY #(SCAN_START_BORDER+HEIGHT_BORDER-1)
    BCC StartBorder
    
;***********************************************************
;       Start Stack Call Danger Zone (Don't use JSR)
;***********************************************************
;   Do not use stack or JSR inside danger zone, 
;    use stack only for print ball

    ; Trick Using Stack Pointer To Print "Ball" (Missile P1)
    LDX #ENAM1
    TXS

; StopBord:
    ; Increment Y-ScanLine Count
    INY
    STA WSYNC
    ; Use Missile P0 to Make Right Border
    STA ENAM0
    LDX #0
    STX PF1
    STX PF2 
    STX CTRLPF ; Control Mode Playfield to Lines
    ; Check Print Ball 
    TYA
    SEC
    SBC BALL_PVERT
    AND #($FF-HEIGHT_BALL)
    PHP
    PLA

WaitStartLines:
    STA WSYNC
    ; Increment Y-ScanLine Count
    INY
    ; Check Print Ball 
    TYA
    SEC
    SBC BALL_PVERT
    AND #($FF-HEIGHT_BALL)
    PHP
    PLA
    CPY #(SCAN_START_LINES-1)
    BCC WaitStartLines
    
; Start Print Lines
    STY COUNT_SCANLINES
    LDX #(NUMBER_LINES-1)
    LDY #(HEIGHT_LINES-1)
    
PrintLines:
    ; Increment Memory ScanLine Count
    INC COUNT_SCANLINES
    STA WSYNC
    LDA COUNT_SCANLINES
    ; Check Print Ball 
    SEC
    SBC BALL_PVERT
    AND #($FF-HEIGHT_BALL)
    PHP
    ; Set Line Color
    LDA LineColors,X
    STA COLUPF
    ; Get PF of lines
    LDA LINES_PFS0,X
    STA PF1
    LDA LINES_PFS1,X
    STA PF2
    LDA LINES_PFS2,X
    STA PF0
    LDA LINES_PFS3,X
    STA PF1
    PLA
    ; Reset For New ScanLine
    LDA #0
    STA PF2
    STA PF0
    ; Check Height of Current Color Line
    DEY
    BPL PrintLines
; Transition Between Different Color Lines
    ; Increment Memory ScanLine Count
    INC COUNT_SCANLINES
    LDY #(HEIGHT_LINES-1)
    STA WSYNC
    LDA COUNT_SCANLINES
    ; Check Print Ball 
    SEC
    SBC BALL_PVERT
    AND #($FF-HEIGHT_BALL)
    PHP
    ; Set Line Color
    LDA LineColors,X
    STA COLUPF
    ; Get PF of lines
    LDA LINES_PFS0,X
    STA PF1
    LDA LINES_PFS1,X
    STA PF2
    LDA LINES_PFS2,X
    STA PF0
    LDA LINES_PFS3,X
    STA PF1
    PLA
    ; Reset For New ScanLine
    LDA #0
    STA PF2
    STA PF0

    DEX
    BPL PrintLines
    
; End of Color Lines
    ; Get Current ScanLine Count
    LDY COUNT_SCANLINES
    STA WSYNC
    STA PF1
    STA PF2
    ; Increment Y-ScanLine Count
    INY
    TYA
    ; Check Print Ball 
    SEC
    SBC BALL_PVERT
    AND #($FF-HEIGHT_BALL)
    PHP
    PLA

WaitStartPlayer: 
    STA WSYNC
    ; Increment Y-ScanLine Count
    INY
    TYA
    ; Check Print Ball 
    SEC
    SBC BALL_PVERT
    AND #($FF-HEIGHT_BALL)
    PHP
    PLA
    CPY #SCAN_POS_PLAYER
    BNE WaitStartPlayer

    STA WSYNC
    ; Enable Print Player
    LDA #$FF
    STA GRP1

    LDX #HEIGHT_PLAYER

    ; Print Player (Missile 1)
PrintPlay:
    ; Increment Y-ScanLine Count
    INY
    TYA
    ; Check Print Ball
    SEC
    SBC BALL_PVERT
    AND #($FF-HEIGHT_BALL)
    PHP
    PLA
    ; Decrease Player Height
    DEX
    STA WSYNC
    BNE PrintPlay
    ; Increment Y-ScanLine Count
    INY
    TYA
    ; Check Print Ball 
    SEC
    SBC BALL_PVERT
    AND #($FF-HEIGHT_BALL)
    PHP
    PLA
    ; Stop to Draw Borders and Player
    ; X:=0
    STX GRP0 
    STX GRP1
    STX ENAM0 
    STX ENABL

;=============================================================================================
;                                     OVERSCAN
;=============================================================================================
    ; Wait hot Scanlines over
ScanlineEnd:
    TYA
    ; Check Print Ball 
    SEC
    SBC BALL_PVERT
    AND #($FF-HEIGHT_BALL)
    PHP
    PLA
    STA WSYNC
    ; Increment Y-ScanLine Count
    INY
    CPY #(KERNEL_SCANLINE-1)
    BNE ScanlineEnd
    
    ; Restore Stack Pointer
    LDX #$FF
    TXS
;***********************************************************
;      End Stack Danger Zone (JSR available)
;***********************************************************

;=============================================================================================
;=============================================================================================
;=============================================================================================
;                                  END OF KERNEL
;=============================================================================================
;=============================================================================================
;=============================================================================================
Overscan:
    LDA #OVERSCAN_TIMER     ; Timing OverScanlines
    STA TIM64T

    INX ; X:=0
    STX ENAM1               ; "Ball" below the screen (stop ball draw )
    STA WSYNC

;===================================================================
;===================================================================
;                      Overscan Code Area
;===================================================================
;===================================================================
    ; Score Test (Auto Increment Every Frame)
    JSR SetInfoDigitPointers
    JSR AddScore

;===================================================================
;                   COLLISION PROCESSING AREA
;===================================================================
; Collision Ball with wall or dead
    ; Ball Alive? (BALL_STATUS:0 == 1)
    LDA BALL_STATUS
    AND #1
    BEQ NoCollision
    ; Dead ball? (Ball BALL_PVERT > KERNEL_SCANLINE)
    LDA BALL_PVERT
    CMP #KERNEL_SCANLINE
    BCC CollPlayer
    LDA #$16
    STA BALL_STATUS
    ; Collision Ball With Player
CollPlayer:
    LDA CXM1P
    AND #$40
    BEQ CollVert
    ; Player Collision set ball vertical move to up 
    LDA #$02
    ORA BALL_STATUS
    STA BALL_STATUS
    ; Collision Ball With Top Bord
CollVert:
    LDY BALL_PVERT
    CPY #(SCAN_START_BORDER+HEIGHT_BORDER+1)
    BCS CollHoriz
    ; Top collision set ball vertical move to down
    LDA #$FD
    AND BALL_STATUS
    STA BALL_STATUS
    ; Collision Ball With Lateral Bords
    ; Check Left Border
CollHoriz:
    LDY BALL_PHORZ
    ; Left
    CPY #25
    BCS CollCheckRight
    ; Left collision set ball horizon move to right
    LDA #$04
    ORA BALL_STATUS
    STA BALL_STATUS
    JMP NoCollision
    ; Check Right Border
CollCheckRight:
    ; Right
    CPY #126
    BCC NoCollision
    ; Right collision set ball horizon move to left
    LDA #$FB
    AND BALL_STATUS
    STA BALL_STATUS

NoCollision:
    LDA #%01000010          ; "Turn Off Cathodic Ray"
    STA VBLANK      

;=============================================================================================
;                                 END OVERSCAN
;=============================================================================================
; Wait Rest of Existing OverScan (Async Clock)
WaintOverscanEnd:           ; Timing OverScanlines
    LDA INTIM
    BNE WaintOverscanEnd
    JMP StartFrame          ; Back to Start  

;=============================================================================================
;=============================================================================================
;             				  FUNCTION DECLARATION
;=============================================================================================
;=============================================================================================

; FUNCTION AjustPointerDigit (A:=A+20*X):
;   Apply n sums of 20 in to register A
;   where n is the value in register x
;   If sum generate carry, Y incremented
AjustPointerDigit:
    CPX #0
    BEQ OutAjust
NoCheckZero:
    CLC
    ADC #20
    BCC NoCarryPointer
    INY
NoCarryPointer:
    DEX
    BNE NoCheckZero
OutAjust:
    RTS

; FUNCTION ResetGame (None):
;   Restore Ball, Life, Color Lines and Score
ResetGame:
    ; Set PF Color Lines (Reset Lines)
    LDX #(NUMBER_LINES-1)
    LDA #$3F
    LDY #$FF
SetPFColorLines:
    STA LINES_PFS0,X 
    STY LINES_PFS1,X
    STY LINES_PFS2,X
    STY LINES_PFS3,X
    DEX
    BPL SetPFColorLines
    INX     ;X:=0

    ; Reset Score
    STX SCORE
    STX SCORE+1
    ; Reset Ball
    STX BALL_STATUS
    LDA #KERNEL_SCANLINE
    STA BALL_PVERT
    ; Reset Life
    LDA #PLAYER_LIFE
    STA COUNT_LIFE
    RTS

; FUNCTION SetInfoDigitPointers (None):
;   Set Score Graph Bitmap Pointers to Current Score and Count Life
SetInfoDigitPointers:
    ; Score Pointer
    ; Get BCD of Digit 0 (Most Significant Digit)
    LDA SCORE
    AND #$F0
    ; Shift for memory pointer
    LDX #4
ShiftScoreDigit0:
    LSR
    DEX
    BNE ShiftScoreDigit0
    ; Adjusts the pointer to the digit, using the function 'AjustPointerDigit' 
    TAX
    CPX #0
    BEQ DigitBlank
    LDA #<Data0
    LDY #>Data0
    JMP DigitAjust
    ; Invisible Digit if Score is Less than 1000.
DigitBlank:
    LDA #<DataEmpty
    LDY #>DataEmpty
DigitAjust:
    JSR AjustPointerDigit
    STA POINTER_SCORE
    STY POINTER_SCORE+1
    ; Get BCD of Digit 2
    LDA SCORE+1
    AND #$F0
    ; Shift for memory pointer
    LDX #4
ShiftScoreDigit2:
    LSR
    DEX
    BNE ShiftScoreDigit2
    ; Adjusts the pointer to the digit, using the function 'AjustPointerDigit' 
    TAX
    LDA #<Data0R
    LDY #>Data0R
    JSR AjustPointerDigit
    STA POINTER_SCORE+4
    STY POINTER_SCORE+5
    ; Get BCD of Digit 1
    LDA SCORE
    AND #$0F
    TAX
    LDA #<Data0
    LDY #>Data0
    JSR AjustPointerDigit
    STA POINTER_SCORE+2
    STY POINTER_SCORE+3
    ; Get BCD of Digit 3
    LDA SCORE+1
    AND #$0F
    ; Adjusts the pointer to the digit, using the function 'AjustPointerDigit' 
    TAX
    LDA #<Data0R
    LDY #>Data0R
    JSR AjustPointerDigit
    STA POINTER_SCORE+6
    STY POINTER_SCORE+7
    ; Ajust Pointer for Count Life
    LDX COUNT_LIFE
    LDA #<Data0
    LDY #>Data0
    JSR AjustPointerDigit
    STA POINTER_LIFE
    STY POINTER_LIFE+1
    RTS

; FUNCTION AddScore (None):
;   Increment one in Score (2 bytes in BCD mode)
AddScore:
    SED
    LDA SCORE+1
    CLC
    ADC #1
    STA SCORE+1
    LDA SCORE
    ADC #0
    STA SCORE
    CLD
    RTS
    
;=============================================================================================
;             				  DATA DECLARATION
;=============================================================================================
;   Colors Lines Hex Code
LineColors:
    ; Load Colors of lines
    .BYTE LINE_COLOR6
    .BYTE LINE_COLOR5
    .BYTE LINE_COLOR4
    .BYTE LINE_COLOR3
    .BYTE LINE_COLOR2
    .BYTE LINE_COLOR1

;   Numbers Graph BitMap
DataEmpty:
    ; Empty
    .BYTE #0,#0,#0,#0,#0,#0,#0,#0,#0,#0
Data0:
    ;0 
    .BYTE #%11101110
    .BYTE #%11101110
    .BYTE #%10101010
    .BYTE #%10101010
    .BYTE #%10101010
    .BYTE #%10101010
    .BYTE #%10101010
    .BYTE #%10101010
    .BYTE #%11101110
    .BYTE #%11101110
Data0R:
    .BYTE #%01110111
    .BYTE #%01110111
    .BYTE #%01010101
    .BYTE #%01010101
    .BYTE #%01010101
    .BYTE #%01010101
    .BYTE #%01010101
    .BYTE #%01010101
    .BYTE #%01110111
    .BYTE #%01110111
    ;1 
    .BYTE #%01000100
    .BYTE #%01000100
    .BYTE #%01000100
    .BYTE #%01000100
    .BYTE #%01000100
    .BYTE #%01000100
    .BYTE #%01000100
    .BYTE #%01000100
    .BYTE #%01000100
    .BYTE #%01000100
    .BYTE #%00100010
    .BYTE #%00100010
    .BYTE #%00100010
    .BYTE #%00100010
    .BYTE #%00100010
    .BYTE #%00100010
    .BYTE #%00100010
    .BYTE #%00100010
    .BYTE #%00100010
    .BYTE #%00100010
    ;2
    .BYTE #%11101110
    .BYTE #%11101110
    .BYTE #%00100010
    .BYTE #%00100010
    .BYTE #%11101110
    .BYTE #%11101110
    .BYTE #%10001000
    .BYTE #%10001000
    .BYTE #%11101110
    .BYTE #%11101110
    .BYTE #%01110111
    .BYTE #%01110111
    .BYTE #%01000100
    .BYTE #%01000100
    .BYTE #%01110111
    .BYTE #%01110111
    .BYTE #%00010001
    .BYTE #%00010001
    .BYTE #%01110111
    .BYTE #%01110111  
    ;3
    .BYTE #%11101110
    .BYTE #%11101110
    .BYTE #%00100010
    .BYTE #%00100010
    .BYTE #%11101110
    .BYTE #%11101110
    .BYTE #%00100010
    .BYTE #%00100010
    .BYTE #%11101110
    .BYTE #%11101110
    .BYTE #%01110111
    .BYTE #%01110111
    .BYTE #%01000100
    .BYTE #%01000100
    .BYTE #%01110111
    .BYTE #%01110111
    .BYTE #%01000100
    .BYTE #%01000100
    .BYTE #%01110111
    .BYTE #%01110111
    ;4
    .BYTE #%10101010
    .BYTE #%10101010
    .BYTE #%10101010
    .BYTE #%10101010
    .BYTE #%11101110
    .BYTE #%11101110
    .BYTE #%00100010
    .BYTE #%00100010
    .BYTE #%00100010
    .BYTE #%00100010
    .BYTE #%01010101
    .BYTE #%01010101
    .BYTE #%01010101
    .BYTE #%01010101
    .BYTE #%01110111
    .BYTE #%01110111
    .BYTE #%01000100
    .BYTE #%01000100
    .BYTE #%01000100
    .BYTE #%01000100
    ;5
    .BYTE #%11101110
    .BYTE #%11101110
    .BYTE #%10001000
    .BYTE #%10001000
    .BYTE #%11101110
    .BYTE #%11101110
    .BYTE #%00100010
    .BYTE #%00100010
    .BYTE #%11101110
    .BYTE #%11101110
    .BYTE #%01110111
    .BYTE #%01110111
    .BYTE #%00010001
    .BYTE #%00010001
    .BYTE #%01110111
    .BYTE #%01110111
    .BYTE #%01000100
    .BYTE #%01000100
    .BYTE #%01110111
    .BYTE #%01110111
    ;6
    .BYTE #%10001000
    .BYTE #%10001000
    .BYTE #%10001000
    .BYTE #%10001000
    .BYTE #%11101110
    .BYTE #%11101110
    .BYTE #%10101010
    .BYTE #%10101010
    .BYTE #%11101110
    .BYTE #%11101110
    .BYTE #%00010001
    .BYTE #%00010001
    .BYTE #%00010001
    .BYTE #%00010001
    .BYTE #%01110111
    .BYTE #%01110111
    .BYTE #%01010101
    .BYTE #%01010101
    .BYTE #%01110111
    .BYTE #%01110111
    ;7
    .BYTE #%11101110
    .BYTE #%11101110
    .BYTE #%00100010
    .BYTE #%00100010
    .BYTE #%00100010
    .BYTE #%00100010
    .BYTE #%00100010
    .BYTE #%00100010
    .BYTE #%00100010
    .BYTE #%00100010
    .BYTE #%01110111
    .BYTE #%01110111
    .BYTE #%01000100
    .BYTE #%01000100
    .BYTE #%01000100
    .BYTE #%01000100
    .BYTE #%01000100
    .BYTE #%01000100
    .BYTE #%01000100
    .BYTE #%01000100
    ;8
    .BYTE #%11101110
    .BYTE #%11101110
    .BYTE #%10101010
    .BYTE #%10101010
    .BYTE #%11101110
    .BYTE #%11101110
    .BYTE #%10101010
    .BYTE #%10101010
    .BYTE #%11101110
    .BYTE #%11101110
    .BYTE #%01110111
    .BYTE #%01110111
    .BYTE #%01010101
    .BYTE #%01010101
    .BYTE #%01110111
    .BYTE #%01110111
    .BYTE #%01010101
    .BYTE #%01010101
    .BYTE #%01110111
    .BYTE #%01110111
    ;9
    .BYTE #%11101110
    .BYTE #%11101110
    .BYTE #%10101010
    .BYTE #%10101010
    .BYTE #%11101110
    .BYTE #%11101110
    .BYTE #%00100010
    .BYTE #%00100010
    .BYTE #%00100010
    .BYTE #%00100010
    .BYTE #%01110111
    .BYTE #%01110111
    .BYTE #%01010101
    .BYTE #%01010101
    .BYTE #%01110111
    .BYTE #%01110111
    .BYTE #%01000100
    .BYTE #%01000100
    .BYTE #%01000100
    .BYTE #%01000100

    ORG $FFFA

    .WORD StartFrame    ; NMI
    .WORD BootGame      ; EntryPoint
    .WORD BootGame      ; IRQ/BRK
END
