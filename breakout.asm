; Para compilar usar o DASM (Multiplataforma e livre !!)
; (http://dasm-dillon.sourceforge.net/), para baixar:
; Roda na linha de comando:
;
;     dasm breakout.asm -obreakout.bin -f3
;

    PROCESSOR 6502
    INCLUDE "vcs.h"
    INCLUDE "macro.h"

;===================================================================
;===================================================================
;           VARIAVEIS GLOBAIS

TOP_BORD        = 8
HEIGHT_BORD     = 19
LIMIT_SCREEN    = 192
HEIGHT_PLAYER   = 4
PLAYER_POS      = LIMIT_SCREEN - HEIGHT_PLAYER
TOP_LINES       = TOP_BORD+HEIGHT_BORD+24
N_CORES         = 6
SCAN_COR        = 7
HEIGHT_LINES    = N_CORES * SCAN_COR
BALL_Size       = 3  ;(2L x 3C)
;===================================================================
;===================================================================
;           VARIAVEIS RAM ($0080-$00FF)(128B RAM)
PosX_Player0      = $0080
Frames_Pass       = $0081
Scan_delay        = $0082
Respawn_P0        = $0083
Cores_Lines       = $0083  ;($0084 -- $0089) (6 Bytes) (6 linhas de cores)
Cont_Cor          = $0090
Ball_posX         = $0091
Ball_Height       = $0092
Ball_posY         = $0093
Ball_wthP0        = $0094
    ; Bit    Description

    ; 0      Player with Ball   (1 - True , 0 - False)
    ; 1      Ball Up            (1 - UP   , 0 - Down)
    ; 2      Ball Right         (1 - Right, 0 - Left)


RANDOM_INC        = $00A0    ; RANDOM FRAME INCREMENT
;===================================================================
;===================================================================

    ORG   $F000       ; Start of "cart area" (see Atari memory map)
    
Boot_Game:
    ; Setando as cores das linhas
    LDY   #1
    LDA   #$48
    STA   Cores_Lines,y
    INY
    LDA   #$38
    STA   Cores_Lines,Y
    INY
    LDA   #$28
    STA   Cores_Lines,Y
    INY
    LDA   #$18
    STA   Cores_Lines,Y
    INY
    LDA   #$D8
    STA   Cores_Lines,Y
    INY
    LDA   #$88
    STA   Cores_Lines,Y
    INY
    ; Setando a posição (Horizontal) inicial do jogador e da ball
    LDA   #62
    STA   PosX_Player0
    STA   Ball_posX
    ; Setando Contador de frames, cores, bool de respawn (Respawn_P0)
    LDA   #0
    STA   Frames_Pass
    STA   Respawn_P0
    STA   Cont_Cor
    STA   ENABL

    STA   COLUBK

    LDA   #$06
    STA   COLUPF
    STA   VBLANK      ; Ativando o VBLANK primeiro load

    STA   COLUP0
    STA   COLUP1

    LDA   #$11
    STA   CTRLPF

    LDA   #$35
    STA   NUSIZ0
    STA   NUSIZ1

    LDA   #$0F
    STA   Ball_wthP0

    LDA   #PLAYER_POS       ; Set "y" for ball
    SBC   #8
    STA   Ball_posY
    ADC   #BALL_Size
    STA   Ball_Height

;=============================================================================================

StartFrame:
    INC   $00A0
    LDA   #33
    STA   Scan_delay
    STA   HMCLR

    LDA   #$02                  ; Vertical sync is signaled by VSYNC's bit 1...
    STA   VSYNC
    REPEAT 3                    ; ...AND lasts 3 scanlines
          STA  WSYNC            ; (WSYNC write => wait for end of scanline)
    REPEND
    LDA   #0
    STA   VSYNC                 ;  Signal vertical sync by clearing the bit

    ; Count Frames IN
    LDY   Frames_Pass
    INY
    CPY   #1                    ; Nº Frames necessário para ativar movimentação.
    BNE   Long_Jump2
    LDY   #0

    LDA   SWCHA
    AND   #$10
    BEQ   Mup

    LDA   SWCHA
    AND   #$20
    BEQ   MDown

    LDA   SWCHA
    AND   #$40
    BEQ   Mleft

    LDA   SWCHA
    AND   #$80
    BEQ   Mright

    LDA   INPT4
    AND   #$80
    BEQ   PrsButton

    ;JMP   Not_move

Long_Jump2:
    JMP   Not_move

Mup:
    LDA   #127                  ; Nível Máximo a direita
    CMP   PosX_Player0
    BCC   Delay
    
    LDA   #$E0
    INC   PosX_Player0
    INC   PosX_Player0
    STA   HMP0

    JMP   Move

MDown:
    LDA   #0                  ; Nível Máximo a esquerda
    CMP   PosX_Player0
    BCS   Delay

    LDA   #$20
    DEC   PosX_Player0
    DEC   PosX_Player0
    STA   HMP0
    JMP   Move

Mleft:
    LDA   #0                  ; Nível Máximo a esquerda
    CMP   PosX_Player0
    BCS   Move

    LDA   #$20
    DEC   PosX_Player0
    DEC   PosX_Player0
    STA   HMP0
    JMP   Move

Mright:
    LDA   #127                 ; Nível Máximo a direita
    CMP   PosX_Player0
    BCC   Not_move
    DEC   Scan_delay
    LDA   #$E0
    INC   PosX_Player0
    INC   PosX_Player0
    STA   HMP0
    JMP   Move

PrsButton:
    LDA   Ball_wthP0
    AND   #$01
    BEQ   Not_move

    DEC   Ball_wthP0
    LDA   $00A0
    AND   #$0F
    STA   WSYNC
    DEC   Scan_delay
Reset_Ball:
    NOP 
    SBC   #1
    BNE   Reset_Ball
    STA   RESBL

    LDA   PosX_Player0
    STA   Ball_posX

    LDA   #PLAYER_POS
    SBC   #8
    STA   Ball_posY
    ADC   #BALL_Size
    STA   Ball_Height
    
    JMP   Not_move

Move:
    STA   WSYNC
    STA   HMOVE
    JMP   Not_move
Delay:
    INC   Scan_delay

Not_move:
    STY   Frames_Pass

    LDA   #0
    ;  Position Respawn (boot only)
    CMP   Respawn_P0           ; LDA #0
    BNE   PreparePlayfield     ; Only one time on
    INC   Respawn_P0

    STA   WSYNC
    SLEEP 41
    STA   RESP0       ;  Position Respawn OUT

    ; Positon the missile 1 to rigth side screen
    LDA   #$40
    STA   HMM1
    STA   WSYNC
    STA   HMOVE
    STA   HMOVE
    ;STA   WSYNC

;=============================================================================================

PreparePlayfield:             ; We'll use the first VBLANK scanline for setup
    LDA   #0
    STA   PF0
    STA   PF1
    STA   PF2
    STA   GRP0

    STA   WSYNC
    STA   HMCLR

    ;   Movimentando a Ball
    LDA   Ball_wthP0
    AND   #1
    BNE   VBlank_Sync_Finished
    LDA   Ball_wthP0

    AND   #$02       ; Vertical Move
    BEQ   B_Down
    DEC   Ball_posY
    DEC   Ball_Height
    JMP   B_horinz

B_Down:
    INC   Ball_posY
    INC   Ball_Height

B_horinz:
    LDA   Ball_wthP0 ;
    AND   #$04       ; HORINZ Move
    BEQ   B_Left
    INC   Ball_posX
    LDA   #$F0       ; Speed Move ball for rigth
    JMP   B_Move

B_Left:
    DEC   Ball_posX
    LDA   #$10       ; Speed Move ball for left

B_Move:
    STA   HMBL       ;   Movimentando a Ball
    DEC   Scan_delay
    
    STA   WSYNC
    STA   HMOVE

VBlank_Sync_Finished:     ; Vblank sync (37 Scanline)
    STA   WSYNC
    DEC   Scan_delay
    BNE   VBlank_Sync_Finished

    LDA   #0                  
    LDX   #0              ; X will count visible scanlines, let's reset it
    LDY   #0
    STA   VBLANK          ; Vertical blank is done, we can "turn on" the beam
;=============================================================================================
;                                KERNEL
;=============================================================================================
;             PRINT SCREEN MOMENT (HOT SCANLINES).
Scanline:
    CPX   #(TOP_BORD)
    BCC   Score_Write  ;<
    BEQ   Start_Bord   ;==
    CPX   #(TOP_BORD+HEIGHT_BORD+2)
    BEQ   Stop_Bord    ;==
    JMP   Logic_game

Start_Bord:
    INX
    STA   WSYNC
    LDA   #$06
    STA   COLUPF
    LDA   #$FF
    STA   PF0
    STA   PF1
    STA   PF2
    LDA   #2
    STA   ENAM0
    STA   ENAM1

    JMP   ScanlineEnd

Score_Write:
    JMP   ScanlineEnd

Stop_Bord:
    INX
    STA   WSYNC
    LDA   #$00
    STA   PF0
    STA   PF1
    STA   PF2
    LDA   #$48
    STA   COLUPF
    LDY   #0
;    JMP   Logic_game

;=============================================================================================
Logic_game:
    CPX   Ball_posY
    BEQ   Write_Ball
    CPX   Ball_Height
    BEQ   Not_Write_Ball

    JMP   N_BALL

Write_Ball:
    LDA   #2
    STA   ENABL
    JMP   N_BALL

Not_Write_Ball:
    LDA   #0
    STA   ENABL
    ;JMP   N_BALL

N_BALL:
    CPX   #TOP_LINES
    BCC   Next
    CPX   #(TOP_LINES+HEIGHT_LINES)
    BEQ   Stop_Lines
    BCS   Print_Player0

    JSR   Print_Lines
    JMP   Logic_game

Stop_Lines:
    INX
    STA   WSYNC
    LDA   #0
    STA   PF0
    STA   PF1
    STA   PF2
    LDA   #$48
    STA   COLUPF

Print_Player0
    CPX   #PLAYER_POS
    BEQ   Write_Player
    CPX   #(PLAYER_POS+HEIGHT_PLAYER-1)
    BEQ   Not_Write_Player

    JMP   Next

Write_Player:
    LDA   #0
    INX
    STA   WSYNC

    STA   ENAM0
    STA   ENAM1

    LDA   #$46
    STA   COLUP0

    LDA   Player_Sprite
    STA   GRP0

    JMP   Print_Player0

Not_Write_Player:
    INX
    STA   WSYNC
    LDA   #$06
    STA   COLUP0
    LDA   #0
    STA   GRP0
Next:

;=============================================================================================
;=============================================================================================
;=============================================================================================

ScanlineEnd:
    INX                 ; Incrementa contador de scanline. Verifica final da tela util.
    STA   WSYNC
    CPX   #(LIMIT_SCREEN+1) ; Ultima Scanline relativa (util).
    BCS   Overscan
    JMP   Scanline
Overscan:
    LDY   #37         ; OVERSCAN LINES
    LDA   #0
    STA   ENABL
    STA   PF0
    STA   PF1
    STA   PF2
    STA   WSYNC
    LDA   #%01000010  ; "turn off"
    STA   VBLANK      ;

      ; Tratamento de Colisão da BALL (quicar)
      ; Colisão com as paredes (Missiles)

    LDA   CXBLPF      ; BALL cw PF
    AND   #$80
    BNE   CPFB

    LDA   CXM0FB      ; BALL cw M0
    AND   #$40
    BNE   CM0B

    LDA   CXM1FB      ; BALL cw M1
    AND   #$40
    BNE   CM1B

    LDA   CXP0FB      ; BALL cw P0
    AND   #$40
    BNE   CP0B

    JMP   N_Colision
CM0B:
    LDA   Ball_wthP0
    ORA   #$04
    STA   Ball_wthP0
    JMP   N_Colision
CM1B:
    LDA   Ball_wthP0
    AND   #$FB
    STA   Ball_wthP0
    JMP   N_Colision
CP0B:
    LDA   Ball_wthP0
    ORA   #$02
    STA   Ball_wthP0
    JMP   N_Colision
CPFB:
    LDA   Ball_wthP0
    AND   #2
    BEQ   EQ

    LDA   Ball_wthP0
    CLC
    SBC   #1
    JMP   NQ
EQ:
    LDA   Ball_wthP0
    ADC   #1
NQ:
    STA   Ball_wthP0
    ;JMP   N_Colision

N_Colision:
    STA   CXCLR
    ; Dead BAll
    LDA   Ball_posY
    CMP   #(LIMIT_SCREEN)
    BEQ   Dead_Ball
    JMP   Sync_delay

Dead_Ball:
    LDA   #0
    STA   ENABL
    LDA   #$07
    STA   Ball_wthP0

Sync_delay:           ; Last 30 Scanline.
    STA   WSYNC
    DEY
    BNE  Sync_delay
    JMP   StartFrame  ; Volta pro main.

;=============================================================================================
;             Functions
;=============================================================================================

Print_Lines:
    LDA   Cont_Cor
    CMP   #SCAN_COR
    BNE   N_Inc
    LDA   #0
    STA  Cont_Cor
    INY
N_Inc:
    INX            ; Incrementa contador de scanline. Verifica final da tela util.
    STA   WSYNC
    LDA   #$FF     ;   LINHAS COLORIDAS !  
    STA   PF0      ;  
    STA   PF1      ;
    STA   PF2      ;
    LDA   Cores_Lines,y
    STA   COLUPF
    INC   Cont_Cor

    RTS
;=============================================================================================
;             DATA DECLARATION
;=============================================================================================

Player_Sprite:
    .BYTE %11111111;

    ORG $FFFC

    .WORD Boot_Game      ;    BOOT

END
