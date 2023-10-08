.label REUBASE  = $df00
.label REUCMD   = $df01

.byte $00,$0c,$08,$0a,$00,$9e,$32,$30,$36,$32,$00,$00,$00,$00

REUBanks:
    .byte $00

REUConfig:
    .byte $00

REUDetect:
    lda #$00
    sta REUBanks
    sta REUBASE
    cmp REUBASE
    beq REUNone
    lda REUBASE
    and #$10
    cmp #$10
    beq !+ // REGCHECK
    lda REUBASE
    and #$10
    cmp #$00
    beq REU1700
    bne REUNone

!: // REGCHECK
    lda REUBASE
    ldx #$02
!: // LOOP1
    txa
    sta REUBASE, x
    inx
    cpx #$05
    bne !- // LOOP1
    ldx #$02
!: // LOOP2
    txa
    cmp REUBASE, x
    bne REUNone
    inx
    cpx #$05
    bne !- // LOOP2

!: // RINIT
    ldx #$00
    lda #$80
    sta REUConfig
    lda #$12
    //sta C64Hi + 1
    //stx REUBanks + 1

    rts

REU1700:
    lda #$01
    rts

REU1764:
    lda #$02
    rts

REU1750:
    lda #$03
    rts

REUNone:
    lda #$00
    rts

