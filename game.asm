; Para compilar usar o DASM (Multiplataforma e livre !!)
; (http://dasm-dillon.sourceforge.net/), para baixar:
; Roda na linha de comando:
;
;     dasm hello.asm -ohello.bin -f3
;

    PROCESSOR 6502
    INCLUDE "vcs.h"

;===================================================================
;===================================================================
;           VARIAVEIS GLOBAIS

TOP_BORD        = 7
START_BOTTON    = 184
LIMIT_SCREEN    = 192
END_BOTTON      = LIMIT_SCREEN - 1

;===================================================================
;===================================================================
;           VARIAVEIS RAM ($0080-$00FF)(128B)


PosY_UP_Player0   = $0080

PosY_DN_Player0   = $0081

PosX_Player0      = $0082

SpriteP0_Control  = $0083

Frames_Pass       = $0084

Scan_delay        = $0085

Respawn_P0        = $0086

Register_user     = $0087
          ;(0-bit - Delay use in moviment H)
          ;(1-bit - Delay use in moviment V)

;===================================================================
;===================================================================

    ORG   $F000       ; Start of "cart area" (see Atari memory map)

Boot_Game:
    ; Setando variáveis na memory RAM
    LDA   #84
    STA   PosY_UP_Player0
    LDA   #107
    STA   PosY_DN_Player0

    LDA   #0
    STA   Frames_Pass
    STA   Respawn_P0
    STA   PosX_Player0

                      ; Setando zero em alguns registradores de "video"
                      ; E/Dis nable(BALL, Missiles and Players)
    STA   ENABL
    STA   ENAM0
    STA   ENAM1
    ;STA   GRP0
    ;STA   GRP1
                      ; Registradores de Movimento Horizontal
    ;STA   HMCLR
    ;STA   HMP0
    ;STA   HMP1
    ;STA   HMM0
    ;STA   HMM1
    ;STA   HMBL
                      ; Color Player AND reset pos register
    ;STA   COLUP0
    ;STA   COLUP1
    STA   RESP0
    STA   RESP1
                      ; Playfield's
    ;STA   PF0         ; Posições de escrita do cenário (Playfield) (Inverse read)
    ;STA   PF1         ; (Normal read).
    ;STA   PF2         ; (Inverse read)
                      ; Cor fundo (BK) e frente (Playfield) e modo do Playfield (repetido ou espelhado)
    STA   COLUBK      ; Background color ($00 = black)
    ;STA   CTRLPF      ; Modo repetido (0), e não espelhado(1).

    LDA   #$CA
    STA   COLUPF      ; Playfield collor

    LDA   #$32
    STA   COLUP0
    LDA   #$C2
    STA   COLUP1

    LDA   #1
    STA   CTRLPF

    ;LDA  #$08
    ;STA  REFP1
    ;LDA  #$05
    ;STA  RESP0

;=============================================================================================

StartFrame:
    LDA   #37
    STA   Scan_delay
    STA   HMCLR

    LDA   #0
    STA   Register_user


    LDA   #$02  ; Vertical sync is signaled by VSYNC's bit 1...
    STA   VSYNC
    REPEAT 3                    ; ...AND lasts 3 scanlines
          STA  WSYNC            ; (WSYNC write => wait for end of scanline)
    REPEND

    ; Count Frames IN
    LDY   Frames_Pass
    INY
    CPY   #1                    ; Nº Frames necessário para ativar movimentação.
    BNE   Long_Jmp
    LDY   #0

;Mup
    LDA   #TOP_BORD             ; Nível Máximo de subida
    ADC   #2
    CMP   PosY_UP_Player0
    BCS   MDown

    LDA   SWCHA
    AND   #$10
    BNE   MDown

    DEC   PosY_UP_Player0
    DEC   PosY_UP_Player0
    DEC   PosY_DN_Player0
    DEC   PosY_DN_Player0

    LDA   #1
    STA   Register_user

MDown:
    LDA   #START_BOTTON         ; Nível Mínimo de Descida
    CMP   PosY_DN_Player0
    BCC   Mleft

    LDA   SWCHA
    AND   #$20
    BNE   Mleft

    INC   PosY_UP_Player0
    INC   PosY_UP_Player0
    INC   PosY_DN_Player0
    INC   PosY_DN_Player0

    LDA   #1
    STA   Register_user

Mleft:
    LDA   PosX_Player0
    CMP   #0
    BEQ   Mright

    LDA   SWCHA
    AND   #$40
    BNE   Mright

    LDA   #$08
    STA   REFP0
    LDA   #$10
    DEC   PosX_Player0
    STA   HMP0

    LDA  #1
    AND  Register_user
    BEQ  Mright
    DEC  Scan_delay

    JMP   Mright

Long_Jmp:
    JMP   Not_move

Mright:
    LDA   PosX_Player0
    CMP   #140
    BCS   PrsButton

    LDA   SWCHA
    AND   #$80
    BNE   PrsButton

    LDA   #00
    STA   REFP0
    LDA   #$F0
    INC   PosX_Player0
    STA   HMP0

    LDA  #1
    AND  Register_user
    BEQ  PrsButton
    DEC  Scan_delay

PrsButton:
    LDA   INPT4
    AND   #$80
    BNE   Not_move
    LDA   COLUPF
    ADC   #$10
    STA   COLUPF              ; Playfield collor

    LDA  #1
    AND  Register_user
    BEQ  Not_move
    DEC  Scan_delay

Not_move:
    STY   Frames_Pass

    LDA   #0
                               ;  Position Respawn IN
    CMP   Respawn_P0           ;  LDA #0
    BNE   N_RespawnP0
    INC   Respawn_P0

    LDY   #$03
    STA   WSYNC

Sync_delay3:
    DEY
    NOP
    BNE   Sync_delay3
    STA   RESP0
                                ;  Position Respawn OUT

  ;  JMP   PreparePlayfield

N_RespawnP0:
    LDA   #0
    STA   VSYNC               ;  Signal vertical sync by clearing the bit

;=============================================================================================

PreparePlayfield:             ; We'll use the first VBLANK scanline for setup
    LDX   #0                  ; X will count visible scanlines, let's reset it
    LDY   #0
    STY   SpriteP0_Control

    LDA   #$FF
    STA   PF0
    STA   PF1
    STA   PF2

    LDA   #0
    STA   GRP0
    STA   WSYNC
    STA   HMOVE
    STA   CXCLR

VBlank_Sync_Finished:           ; Vblank sync (37 Scanline)
    STA   WSYNC
    DEC   Scan_delay
    BNE   VBlank_Sync_Finished

    LDA   #0                    ; Vertical blank is done, we can "turn on" the beam
    STA   VBLANK
    INX
    STA   WSYNC
;=============================================================================================
;=============================================================================================
;             PRINT SCREEN MOMENT (HOT SCANLINES).

Scanline:
; Bords print
Bord_botton:
    ; bord_botton in
    CPX   #START_BOTTON
    BCC   Out_bord
    CPX   #END_BOTTON
    BCS   Out_bord

    INX
    STA   WSYNC

    LDA   #$FF
    STA   PF0
    STA   PF1
    STA   PF2
    JMP   ScanlineEnd
    ; bord_botton out

Out_bord:
    CPX   #TOP_BORD
    BCC   ScanlineEnd
    CPX   #(TOP_BORD+1)
    BCC   Lateral_bord
    CPX   #START_BOTTON
    BCC   Logic_game

    JMP   ScanlineEnd

Lateral_bord:
    INX                         ; Incrementa contador de scanline. Verifica final da tela util.
    STA   WSYNC

    LDA   #0
    STA   PF1
    STA   PF2
    LDA   #$10
    STA   PF0

;=============================================================================================
Logic_game:
    ;
    ;  Tape code here (inter top AND botton bord)
    ;



N_Collison:

    ;Print_Player0
    INX                         ; Incrementa contador de scanline. Verifica final da tela util.
    STA   WSYNC

    CPX   PosY_UP_Player0
    BCC   Not_Print_player0
    CPX   PosY_DN_Player0
    BCS   Not_Print_player0

    LDY   SpriteP0_Control
    INC   SpriteP0_Control
    LDA   Player_Sprite,y
    STA   GRP0
;    STA   RESP0

Not_Print_player0:

    JMP   Scanline
;=============================================================================================
;=============================================================================================
;=============================================================================================

ScanlineEnd:
    INX                 ; Incrementa contador de scanline. Verifica final da tela util.
    STA   WSYNC
    CPX   #LIMIT_SCREEN ; Ultima Scanline relativa absoluta (util).
    BNE   Scanline

    LDA   #0
    STA   PF0
    STA   PF1
    STA   PF2
    STA   GRP0
    STA   WSYNC

Overscan:
    LDY   #30
    LDA   #%01000010  ; "turn off"
    STA   VBLANK      ;
Sync_delay4:          ; Last 30 Scanline.
    STA   WSYNC
    DEY
    BNE  Sync_delay4

    JMP   StartFrame  ; Volta pro main.

;=============================================================================================
;             Functions
;=============================================================================================


;=============================================================================================
;             DATA DECLARATION
;=============================================================================================

Player_Sprite:
    .BYTE %00111000
    .BYTE %00111000
    .BYTE %00111000
    .BYTE %00111000
    .BYTE %00010000
    .BYTE %00010000
    .BYTE %00111000
    .BYTE %00111000
    .BYTE %01011100
    .BYTE %01011100
    .BYTE %10011010
    .BYTE %10011010
    .BYTE %00011001
    .BYTE %00011001
    .BYTE %00010100
    .BYTE %00010100
    .BYTE %00110010
    .BYTE %00110010
    .BYTE %00100010
    .BYTE %00100010
    .BYTE %00100010
    .BYTE %00100010
    .BYTE %00000000;

    ORG $FFFA

    .WORD Boot_Game      ;     NMI
    .WORD Boot_Game      ;     RESET (BOOTLOADER)
    .WORD Boot_Game      ;     IRQ   (RESET)

END
