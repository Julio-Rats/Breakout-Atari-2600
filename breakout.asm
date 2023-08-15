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
KERNEL_SCANLINE     = 190
SCAN_START_BORDER   = 14
HEIGHT_LINES        = 6

VBLANK_TIMER        = 43
OVERSCAN_TIMER      = 38

BG_COLOR            = $04
PLAYER_COLOR        = $46

LINE_COLOR1         = $46
LINE_COLOR2         = $36
LINE_COLOR3         = $26
LINE_COLOR4         = $16
LINE_COLOR5         = $C6
LINE_COLOR6         = $86
;===================================================================
;                       PAL
; KERNEL_SCANLINE     = 226
; SCAN_START_BORDER   = 22
; HEIGHT_LINES        = 8

; VBLANK_TIMER        = 52
; OVERSCAN_TIMER      = 45

; BG_COLOR            = $08
; PLAYER_COLOR        = $46

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
BALL_SIZE           = 3
SPEED_LEFT          = 3
SPEED_LEFT_HEX      = $30
SPEED_RIGHT         = 3
SPEED_RIGHT_HEX     = $D0
SCAN_POS_PLAYER     = (KERNEL_SCANLINE - HEIGHT_PLAYER)
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
BALL_POS        ds  1
BALL_STATUS     ds  1
;   BALL_STATUS DECODER
;      BIT         ACTION
;       0           LIVE?
;       1           MOVE_RIGHT?
;       2           MOVE_UP?
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

    LDA #68
    STA BALL_POS
    LDA #$16
    STA BALL_STATUS
    LDA #$F0
    STA HMP0
    LDA #$20
    STA HMP1
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

    STX VSYNC           ; Signal vertical sync by clearing the bit

    LDA #VBLANK_TIMER   ; Timing Vblank (37 Scanlines)
    STA TIM64T
    
;===================================================================
;                     Vblank code area
;===================================================================
    LDA #$80
    AND INPT4
    BEQ FIRE
    AND INPT5
    BEQ FIRE
    JMP Controllers

FIRE:
    LDA BALL_STATUS
    AND #1
    BNE Controllers
    INC BALL_STATUS
    LDA #$17
    AND BALL_STATUS
    STA BALL_STATUS

Controllers:
    LDX SWCHA
    ; Left
    TXA
    AND #$40
    BEQ Move_left
    TXA
    AND #$04
    BEQ Move_left
    TXA
    AND #$80
    BEQ Move_right
    TXA
    AND #$08
    BEQ Move_right
    JMP No_move

Move_left:
    LDX BALL_POS
    CPX #25
    BMI No_move
    LDA #SPEED_LEFT_HEX
    STA HMP1
    LDA BALL_POS
    SEC
    SBC #SPEED_LEFT
    STA BALL_POS
    JMP No_move
Move_right: 
    LDX BALL_POS
    CPX #112
    BPL No_move
    LDA #SPEED_RIGHT_HEX
    STA HMP1
    LDA BALL_POS
    CLC
    ADC #SPEED_RIGHT
    STA BALL_POS
No_move:

; PreparePlayfield:   ; We'll use the first VBLANK scanline for setup
    LDA #0
    STA PF0
    STA PF1
    STA PF2
    STA GRP0
    STA GRP1
    STA ENAM0 
    STA ENAM1
    STA ENABL
    STA WSYNC
    STA COUNT_SCANLINES
    LDA #BG_COLOR
    STA COLUPF
    LDA #$21
    STA CTRLPF          ; Control Mode Playfield to Board

WaitVblankEnd:
    LDA INTIM
    BNE WaitVblankEnd

    STA WSYNC
    STA HMOVE
    STA WSYNC
    STA HMCLR
    STA VBLANK          ; Stop Vblank

;=============================================================================================
;                                      KERNEL
;=============================================================================================
;                         PRINT SCREEN MOMENT (HOT SCANLINES)

; Start Visible Scanlines:

; Print Score
StartScore:   
    JSR NewLine
    CPY #(SCAN_START_BORDER-1)
    BCC StartScore

; Print Border:
    LDA #$FF
    LDX #$3F
    STA WSYNC
    STA GRP0
    STX PF1
    STA PF2 
    INC COUNT_SCANLINES
StartBorder:
    JSR NewLine
    CPY #(SCAN_START_BORDER+HEIGHT_BORDER-1)
    BCC StartBorder
    
; StopBoard:
    INC COUNT_SCANLINES
    STA WSYNC
    LDA #$FF
    LDX #0
    STX PF1
    STX PF2 
    STA ENAM0
    LDA #$20
    STA CTRLPF ; Control Mode Playfield to Lines

WaitStartLines:
    JSR NewLine
    CPY #(SCAN_START_LINES-1)
    BCC WaitStartLines

; Print Lines
    LDX #(NUMBER_LINES-1)
    LDY #(HEIGHT_LINES-1)
PrintLines:
    INC COUNT_SCANLINES
    STA WSYNC
    LDA LINE_COLORS,X
    STA COLUPF
    LDA LINES_PFS0,X
    STA PF1
    LDA LINES_PFS1,X
    STA PF2
    LDA LINES_PFS2,X
    NOP
    STA PF0
    LDA LINES_PFS3,X
    NOP
    NOP
    STA PF1
    LDA #0
    NOP
    NOP
    STA PF2
    STA PF0

    DEY
    BPL PrintLines
    LDY #(HEIGHT_LINES-1)
    DEX
    BPL PrintLines

; Stop Lines
    STA WSYNC
    STA PF1
    STA PF2
    INC COUNT_SCANLINES

WaitStartPlayer:  
    JSR NewLine
    CPY #SCAN_POS_PLAYER
    BNE WaitStartPlayer

    INC COUNT_SCANLINES
    LDA #$FF
    STA WSYNC
    STA GRP1

    LDX #HEIGHT_PLAYER
PrintPlay:
    INC COUNT_SCANLINES
    STA WSYNC
    DEX
    BNE PrintPlay
    STX GRP1

;=============================================================================================
;=============================================================================================
;=============================================================================================
    LDY COUNT_SCANLINES
    CPY #KERNEL_SCANLINE
    BNE Overscan
ScanlineEnd:
    JSR NewLine
    CPY #KERNEL_SCANLINE
    BCC ScanlineEnd

Overscan:
    LDA #0
    STA GRP0
    STA ENAM0 
    LDA #OVERSCAN_TIMER     ; Timing OverScanlines
    STA TIM64T

    STA WSYNC
    LDA #%01000010          ; "Turn Off Cathodic Ray and "
    STA VBLANK      

WaintOverscanEnd:           ; Timing 27 Scanline.
    LDA INTIM
    BNE WaintOverscanEnd
    JMP StartFrame          ; Back to Start  
;=============================================================================================
;             				Functions
;=============================================================================================

NewLine:
    INC COUNT_SCANLINES
    LDY COUNT_SCANLINES
    STA WSYNC
    RTS

;=============================================================================================
;             				DATA DECLARATION
;=============================================================================================

    ORG $FFFA

    .WORD StartFrame    ; NMI
    .WORD BootGame      ; EntryPoint
    .WORD BootGame      ; IRQ/BRK
END
