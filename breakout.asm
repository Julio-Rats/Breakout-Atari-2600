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
NUMBER_LINES        = 6 
SCAN_START_BORDER   = 14
LAST_SCANLINE       = 189
HEIGHT_BORDER       = 16
HEIGHT_PLAYER       = 4
HEIGHT_LINES        = 12
BALL_SIZE           = 3 
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

;===================================================================
;===================================================================
;                       CODE SEGMENT

    SEG   CODE
    ORG   $F000     ; Start of "Cart Area" (See Atari Memory Map)

; CPU ENTERPOINT
BootGame:
    SEI
    CLD
    LDA #00
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
    LDA #$04
    STA COLUP0
    STA COLUPF
    LDA #$18
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

    LDA #$F0
    STA HMP0
    LDA #$20
    STA HMP1
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
    LDA #$46
    STA LINE_COLORS+5
    LDA #$36
    STA LINE_COLORS+4
    LDA #$26
    STA LINE_COLORS+3
    LDA #$16
    STA LINE_COLORS+2
    LDA #$c6
    STA LINE_COLORS+1
    LDA #$86
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

;=============================================================================================
; NEW FRAME CYCLE
StartFrame:
    LDA #$02        ; Vertical sync is signaled by VSYNC's bit 1...
    STA VSYNC
    LDX #03

WsynWait:           ; ...waiting 3 scanlines
    STA WSYNC       ; (WSYNC write => wait for end of scanline)
    DEX
    BNE WsynWait

    STX VSYNC       ; Signal vertical sync by clearing the bit

    LDA #47         ; Timing Vblank (37 Scanlines)
    STA TIM64T
    
;=============================================================================================
; interframe code here

    LDA #$21
    STA CTRLPF ; Control Mode Playfield to Board

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
    STA HMCLR
    STA COUNT_SCANLINES
    LDA #$04
    STA COLUPF

WaitVblankEnd:
    LDA INTIM
    BNE WaitVblankEnd

    STA WSYNC
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
PrintPlay
    INC COUNT_SCANLINES
    STA WSYNC
    DEX
    BNE PrintPlay
    STX GRP1

;=============================================================================================
;=============================================================================================
;=============================================================================================
    LDY COUNT_SCANLINES
    CPY #LAST_SCANLINE
    BNE Overscan
ScanlineEnd:
    JSR NewLine
    CPY #LAST_SCANLINE
    BCC ScanlineEnd

Overscan:
    LDA #0
    STA GRP0
    STA ENAM0 
    LDA #37                 ; Timing OverScanlines
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
