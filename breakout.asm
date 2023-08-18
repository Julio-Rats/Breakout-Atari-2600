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
;===================================================================
;                   Constantes

;===================================================================
;                       NTSC
KERNEL_SCANLINE     = 192
SCAN_START_BORDER   = 14
HEIGHT_LINES        = 6

VBLANK_TIMER        = 43
OVERSCAN_TIMER      = 41

BG_COLOR            = $04
PLAYER_COLOR        = $F4

LINE_COLOR1         = $46
LINE_COLOR2         = $36
LINE_COLOR3         = $26
LINE_COLOR4         = $16
LINE_COLOR5         = $C6
LINE_COLOR6         = $86
;===================================================================
;                       PAL
; KERNEL_SCANLINE     = 228
; SCAN_START_BORDER   = 22
; HEIGHT_LINES        = 8

; VBLANK_TIMER        = 52
; OVERSCAN_TIMER      = 45

; BG_COLOR            = $08
; PLAYER_COLOR        = $44

; LINE_COLOR1         = $62
; LINE_COLOR2         = $64
; LINE_COLOR3         = $46
; LINE_COLOR4         = $26
; LINE_COLOR5         = $56
; LINE_COLOR6         = $D6
;===================================================================
NUMBER_LINES        = 6 
HEIGHT_BORDER       = 16
HEIGHT_PLAYER       = 4
HEIGHT_BALL         = 3
SPEED_LEFT          = 3
SPEED_LEFT_HEX      = $30
SPEED_RIGHT         = 3
SPEED_RIGHT_HEX     = $D0
LAST_SCANLINE       = KERNEL_SCANLINE-3
SCAN_POS_PLAYER     = (LAST_SCANLINE - HEIGHT_PLAYER)
SCAN_START_LINES    = (SCAN_START_BORDER + HEIGHT_BORDER + 21)

;===================================================================
;===================================================================
;           VARIAVEIS RAM ($0080-$00FF)(128B RAM)

    SEG.U VARIABLES
    ORG   $80

COUNT_SCANLINES ds  1
LINE_COLORS     ds  NUMBER_LINES
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
;===================================================================
;===================================================================
;                       CODE SEGMENT

    SEG   CODE
    ORG   $F000     ; Start of "Cart Area" (See Atari Memory Map)
;===================================================================
;                       CPU ENTERPOINT
;===================================================================
BootGame:
    SEI
    CLD
    LDA #0
    TAY
    LDX #$FF
    TXS
    INX

ClearMemory:
    DEX
    STA $CC,X
    BNE ClearMemory
    ; Set Color
    STA COLUBK
    LDA #BG_COLOR
    STA COLUP0
    STA COLUPF
    LDA #PLAYER_COLOR
    STA COLUP1
    ; Set Position of P0,P1,M0
    LDY #$04
    STA WSYNC
    DEY
PosPlayer0:
    DEY
    BPL PosPlayer0
    NOP
    STA RESP0

    LDY #$05
PosMissile0:
    DEY
    BPL PosMissile0
    NOP
    NOP
    STA RESM0
    ; Set Position of P0 and M0
    LDY #$08
    STA WSYNC
    DEY
PosPlayer1:
    DEY
    BPL PosPlayer1
    STA RESP1
    STA RESM1

    LDA #68
    STA PLAYER_POS
    LDA #75
    STA BALL_PHORZ
    LDA #KERNEL_SCANLINE
    STA BALL_PVERT
    LDA #$16
    STA BALL_STATUS

    LDA #$F0
    STA HMP0
    LDA #$20
    STA HMP1
    STA HMM1
    STA WSYNC
    STA WSYNC
    STA HMOVE
    STA WSYNC
    STA HMCLR

    ; Set Size and Graph type
    LDA #$30
    STA NUSIZ0
    LDA #$15
    STA NUSIZ1

    ; Load Colors of lines
    LDA #LINE_COLOR1
    STA LINE_COLORS+5
    LDA #LINE_COLOR2
    STA LINE_COLORS+4
    LDA #LINE_COLOR3
    STA LINE_COLORS+3
    LDA #LINE_COLOR4
    STA LINE_COLORS+2
    LDA #LINE_COLOR5
    STA LINE_COLORS+1
    LDA #LINE_COLOR6
    STA LINE_COLORS
    
    ; Set Lines
    LDA #$3F
    LDX #(NUMBER_LINES-1)
SideLeftPF:
    STA LINES_PFS0,X 
    DEX
    BPL SideLeftPF

    LDA #$FF
    LDX #(NUMBER_LINES-1)
OuthersPFS:
    STA LINES_PFS1,X
    STA LINES_PFS2,X
    STA LINES_PFS3,X
    DEX
    BPL OuthersPFS

    LDA #%01000010  ; Starting Vblank
    STA VBLANK  

;===================================================================
;                     NEW FRAME CYCLE
;===================================================================
StartFrame:
    LDA #$02            ; Vertical sync is signaled by VSYNC's bit 1...
    STA VSYNC

    LDX #03
WsynWait:               ; ...waiting 3 scanlines
    STA WSYNC           ; (WSYNC write => wait for end of scanline)
    DEX
    BNE WsynWait

    STX VSYNC           ; Signal vertical sync by clearing the bit (Start Vblank)

    LDA #VBLANK_TIMER   ; Timing Vblank Scanlines
    STA TIM64T
    
;===================================================================
;                     Vblank code area
;===================================================================
    LDA #$80
    AND INPT4
    BEQ Fire
    AND INPT5
    BEQ Fire
    JMP Controllers

Fire:
    LDA BALL_STATUS
    AND #1
    BNE Controllers
    LDA #$17
    STA BALL_STATUS

Controllers:
    LDX SWCHA
    ; Left
    TXA
    AND #$40
    BEQ MovePLeft
    TXA
    AND #$04
    BEQ MovePLeft
    TXA
    AND #$80
    BEQ MovePRight
    TXA
    AND #$08
    BEQ MovePRight
    JMP NoMove

MovePLeft:
    LDX PLAYER_POS
    CPX #25
    BMI NoMove
    LDA #SPEED_LEFT_HEX
    STA HMP1
    LDA PLAYER_POS
    SEC
    SBC #SPEED_LEFT
    STA PLAYER_POS
    JMP NoMove
MovePRight: 
    LDX PLAYER_POS
    CPX #112
    BPL NoMove
    LDA #SPEED_RIGHT_HEX
    STA HMP1
    LDA PLAYER_POS
    CLC
    ADC #SPEED_RIGHT
    STA PLAYER_POS
NoMove:


; Ball Moviment
    LDA BALL_STATUS
    TAY
    AND #1
    BEQ NoMoveBall
    ;Move
    TYA
    LSR
    LSR
    LSR
    LSR
    TAX
    ;MoveVert
    TYA
    AND #2
    BEQ MoveUP
; MoveDown:
    TXA
    EOR #$FF
    SEC
    ADC BALL_PVERT

    JMP MoveHorz
MoveUP:
    TXA
    CLC
    ADC BALL_PVERT
MoveHorz:
    STA BALL_PVERT 

    TYA
    AND #4
    BEQ MoveBLeft
    ;MoveBRight
    TXA
    CLC
    ADC BALL_PHORZ
    STA BALL_PHORZ
    TYA
    AND #$F0
    EOR #$F0
    CLC
    ADC #$10
    JMP Moved
MoveBLeft:
    TXA
    EOR #$FF
    SEC
    ADC BALL_PHORZ
    STA BALL_PHORZ
    TYA
    AND #$F0
Moved:
    STA HMM1
NoMoveBall:

; PreparePlayfield:     ; Preparing graph registers to start hot scanlines
    LDA #BG_COLOR
    STA COLUPF
    LDA #$11
    STA CTRLPF          ; Control Mode Playfield to Board (PlayField Reflect)

WaitVblankEnd:
    LDA INTIM
    BNE WaitVblankEnd

    TAY
    STA WSYNC
    STA HMOVE
    STA CXCLR 
    STA WSYNC
    STA HMCLR
    STA VBLANK          ; Stop Vblank

;=============================================================================================
;                                      KERNEL
;=============================================================================================
;                         PRINT SCREEN MOMENT (HOT SCANLINES)

; Start Visible Scanlines:
    LDX #ENAM1
    TXS
; Print Score
StartScore:   
    INY
    STA WSYNC
    CPY #(SCAN_START_BORDER-1)
    BCC StartScore

; Print Border:
    LDA #$FF
    LDX #$3F
    INY
    STA WSYNC
    STA GRP0
    STX PF1
    STA PF2 

StartBorder:
    INY
    STA WSYNC
    CPY #(SCAN_START_BORDER+HEIGHT_BORDER-1)
    BCC StartBorder
    
; StopBoard:
    INY
    STA WSYNC
    LDA #$FF
    LDX #0
    STX PF1
    STX PF2 
    STA ENAM0
    LDA #$10
    STA CTRLPF ; Control Mode Playfield to Lines

WaitStartLines:
    STA WSYNC
    INY
    TYA
    SEC
    SBC BALL_PVERT
    AND #($FF-HEIGHT_BALL)
    PHP
    PLA
    CPY #(SCAN_START_LINES-1)
    BCC WaitStartLines
    
    STY COUNT_SCANLINES
; Print Lines
    LDX #(NUMBER_LINES-1)
    LDY #(HEIGHT_LINES-1)

PrintLines:
    INC COUNT_SCANLINES
    STA WSYNC
    LDA COUNT_SCANLINES
    SEC
    SBC BALL_PVERT
    AND #($FF-HEIGHT_BALL)
    PHP
    LDA LINE_COLORS,X
    STA COLUPF
    LDA LINES_PFS0,X
    STA PF1
    LDA LINES_PFS1,X
    STA PF2
    LDA LINES_PFS2,X
    STA PF0
    LDA LINES_PFS3,X
    STA PF1
    PLA
    LDA #0
    STA PF2
    STA PF0

    DEY
    BPL PrintLines
    LDY #(HEIGHT_LINES-1)
    
    INC COUNT_SCANLINES
    STA WSYNC
    LDA COUNT_SCANLINES
    SEC
    SBC BALL_PVERT
    AND #($FF-HEIGHT_BALL)
    PHP
    LDA LINE_COLORS,X
    STA COLUPF
    LDA LINES_PFS0,X
    STA PF1
    LDA LINES_PFS1,X
    STA PF2
    LDA LINES_PFS2,X
    STA PF0
    LDA LINES_PFS3,X
    STA PF1
    PLA
    LDA #0
    STA PF2
    STA PF0

    DEX
    BPL PrintLines

    LDY COUNT_SCANLINES
; Stop Lines
    STA WSYNC
    STA PF1
    STA PF2
    TYA
    SEC
    SBC BALL_PVERT
    AND #($FF-HEIGHT_BALL)
    PHP
    PLA

WaitStartPlayer:  
    STA WSYNC
    INY
    TYA
    SEC
    SBC BALL_PVERT
    AND #($FF-HEIGHT_BALL)
    PHP
    PLA
    CPY #SCAN_POS_PLAYER
    BNE WaitStartPlayer

    STA WSYNC
    LDA #$FF
    STA GRP1

    LDX #HEIGHT_PLAYER

PrintPlay:
    INY
    TYA
    SEC
    SBC BALL_PVERT
    AND #($FF-HEIGHT_BALL)
    PHP
    PLA
    DEX
    STA WSYNC
    BNE PrintPlay
    INY
    TYA
    SEC
    SBC BALL_PVERT
    AND #($FF-HEIGHT_BALL)
    PHP
    PLA
    STX GRP0
    STX GRP1
    STX ENAM0 

;=============================================================================================
;                                  OVERSCAN
;=============================================================================================
    
ScanlineEnd:
    TYA
    SEC
    SBC BALL_PVERT
    AND #($FF-HEIGHT_BALL)
    PHP
    PLA
    STA WSYNC
    INY
    CPY #(KERNEL_SCANLINE-1)
    BNE ScanlineEnd
    
Overscan:
    LDA #OVERSCAN_TIMER     ; Timing OverScanlines
    STA TIM64T

    LDA #0
    STA ENAM1               ; "Ball" dead
    STA WSYNC

;===================================================================
;                     Overscan code area
;===================================================================

; Colision Ball with wall and dead
    LDA BALL_STATUS
    AND #1
    BEQ NoCollision
    ; Dead Ball?
    LDA BALL_PVERT
    CMP #KERNEL_SCANLINE
    BCC CollPlayer
    LDA #$16
    STA BALL_STATUS
CollPlayer:
    LDA CXM1P
    AND #$40
    BEQ CollVert
    ; Player Collision
    LDA #$02
    ORA BALL_STATUS
    STA BALL_STATUS

CollVert:
    LDY BALL_PVERT
    CPY #(SCAN_START_BORDER+HEIGHT_BORDER+2)
    BCS CollHoriz
    ; Top collision
    LDA #$FD
    AND BALL_STATUS
    STA BALL_STATUS

CollHoriz:
    LDY BALL_PHORZ
    ; Left
    CPY #25
    BPL CollCheckRight
    LDA #$04
    ORA BALL_STATUS
    STA BALL_STATUS

    JMP NoCollision

CollCheckRight:
    ; Right
    CPY #126
    BMI NoCollision
    LDA #$FB
    AND BALL_STATUS
    STA BALL_STATUS

NoCollision:

    LDA #%01000010          ; "Turn Off Cathodic Ray"
    STA VBLANK      

WaintOverscanEnd:           ; Timing OverScanlines
    LDA INTIM
    BNE WaintOverscanEnd
    JMP StartFrame          ; Back to Start  

;=============================================================================================
;             				DATA DECLARATION
;=============================================================================================

    ORG $FFFA

    .WORD StartFrame    ; NMI
    .WORD BootGame      ; EntryPoint
    .WORD BootGame      ; IRQ/BRK
END
