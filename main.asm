*=$c000

#import "include/all.asm"

.label DATA     = $83
.label REM      = $8f
.label PRINT    = $99
.label QUOTE    = $22
.label BUFFER   = $200


// Set these to the start/end tokens for commands and functions.
// You can find these at the bottom of this file.
.label CMDSTART = $cc
.label CMDEND   = $d3
.label FUNSTART = CMDEND + $01
.label FUNEND   = $d5

/*

    Set up our vectors

*/
Init:
    ldx #<ConvertToTokens
    ldy #>ConvertToTokens
    stx vectors.ICRNCH
    sty vectors.ICRNCH + $01

    ldx #<ConvertFromTokens
    ldy #>ConvertFromTokens
    stx vectors.IQPLOP
    sty vectors.IQPLOP + $01

    ldx #<ExecuteCommand
    ldy #>ExecuteCommand
    stx vectors.IGONE
    sty vectors.IGONE + $01

    ldx #<ExecuteFunction
    ldy #>ExecuteFunction
    stx vectors.IEVAL
    sty vectors.IEVAL + $01

    rts

#import "memory.asm"    // Memory commands

/*

    Command Execution. This is the meat of it. This is where the magic happens.
    We first have to figure out whether we're executing one of our custom functions. We
    do this by looking at the current token that's returned by CHRGET. If this token
    is in the range of CMDSTART to CMDEND, then it's one of ours and we should find its
    routine in a lookup table. If it's not, then just perform the normal BASIC command
    handler.

*/
ExecuteCommand:
    jsr zp.CHRGET
    jsr TestCmd
    jmp basic.NEWSTT
TestCmd:
    cmp #CMDSTART
    bcc OldCmd
    cmp #CMDEND + 1
    bcc OkNew
OldCmd:
    jsr zp.CHRGOT
    jmp basic.EXECOLD
OkNew:
    sec
    sbc #CMDSTART
    asl
    tax
    lda CmdTab+1, x
    pha
    lda CmdTab, x
    pha
    jmp zp.CHRGET

/*

    Function Execution. This is the meat of it. This is where the magic happens.
    We first have to figure out whether we're executing one of our custom functions. We
    do this by looking at the current token that's returned by CHRGET. If this token
    is in the range of FUNSTART to FUNEND, then it's one of ours and we should find its
    routine in a lookup table. If it's not, then just perform the normal BASIC function
    handler.

*/
ExecuteFunction:
    lda #0
    sta zp.VALTYP
    jsr zp.CHRGET
    cmp #'$'                    // Is this a HEX number?
    beq ProcessHex
    cmp #'%'                    // Is this a binary number?
    beq ProcessBinary
    cmp #FUNSTART               // Is this one of ours?
    bcc OldFun
    cmp #FUNEND + 1
    bcc Ok1New
OldFun:
    jsr zp.CHRGOT               // It's a built-in Commodore function, so re-fetch the token
    jmp basic.FUNCTOLD          // and call the normal BASIC function handler.
Ok1New:
    sec
    sbc #FUNSTART               // We need to get an index to the function in the vector table
    asl                         // Start by subtracting to get a 0 based index, and then mult by 2.
    pha
    jsr zp.CHRGET
    jsr basic.PARCHK            // Grab whatever is in parens and evaluate it. This is passed to our function.
    pla
    tay
    lda FunTab, y               // Get the function's vector address...
    sta zp.JMPER + 1
    lda FunTab + 1, y
    sta zp.JMPER + 2
    jsr zp.JMPER                // ... and then call it.
    rts

/*

    Allow us to represent numbers as HEX using $ABCD syntax

*/
ProcessHex:
    jsr ClearFAC
!:
    jsr zp.CHRGET
    bcc !+
    cmp #'A'
    bcc !+++
    cmp #'F' + 1
    bcs !+++
    sec
    sbc #$07
!:
    sec
    sbc #'0'
    pha
    lda zp.EXP
    beq !+
    clc
    adc #$04
    bcs !+++
    sta zp.EXP
!:
    pla
    beq !---
    jsr basic.FINLOG
    jmp !---
!:
    jmp zp.CHRGOT
!:
    jmp basic.OVERR

/*

    Allow us to represent numbers as binary using %1010101 syntax

*/
ProcessBinary:
    jsr ClearFAC
!:
    jsr zp.CHRGET
    cmp #'2'
    bcs !---
    cmp #'0'
    bcc !---
    sbc #'0'
    pha
    lda zp.EXP
    beq !+
    inc zp.EXP
    beq !+
!:
    pla
    beq !--
    jsr basic.FINLOG
    jmp !--

ClearFAC:
    lda #$00
    ldx #$0a
!:
    sta zp.FLOAT, x
    dex
    bpl !-
    rts

CmdTab:                         // A table of vectors pointing at your commands' execution addresses
    .word BorderCmd - 1         // Address - 1 of first command. Token = CMDSTART
    .word BackgroundCmd - 1
    .word WokeCmd - 1
    .word ClsCmd - 1
    .word MemCopyCmd - 1
    .word MemFillCmd - 1
    .word ScreenCmd - 1
    .word BankCmd - 1

FunTab:                         // A table of vectors pointing at your functions' execution addresses
    .word WeekFun               // Address of first function. Token = FUNSTART
    .word ScrLocFun

/*

    Clear the screen using PETSCII $93. Easy peasey.

    Example: CLS

*/
ClsCmd:
    lda #$93
    jmp kernal.VEC_CHROUT

/*

    Set the border color

    Example: BORDER 0. Sets the border color to black

*/
BorderCmd:
    jsr GetColor
    sta vic.EXTCOL
    rts

/*

    Set the background color

    Example: BACKGROUND 0. Sets the background color to black

*/
BackgroundCmd:
    jsr GetColor
    sta vic.BGCOL0
    rts

/*

    Execute a POKE but allow passing in a word for the address and the value.

    Example: WOKE 250, 65535. Puts 255 in location 250 and 251.

*/
WokeCmd:
    jsr Get16Bit        // Get the address to WOKE to
    lda $14
    sta $57
    lda $15
    sta $58

    jsr basic.CHKCOM    // Make sure we have a comma

    jsr Get16Bit        // Get the 16-bit word to WOKE

    ldy #$00            // WOKE IT!
    lda $14
    sta ($57), y
    lda $15
    iny
    sta ($57), y

    rts

/*

    Select the VIC bank

    Example: BANK 0 would specify 0-16384 as the range of locations that the VIC sees

*/
BankCmd:
    jsr Get8Bit         // Get the bank #
    tya
    pha
    and #$fc            // Strip the bottom 2 bits.
    cmp #$00            // Is the value > 0? That means we specified a number > 3
    bne !+              // Illegal quantity
    pla
    sta r0L
    lda #$03            // The bit patterns are backwards. 11 means bank 0 and 00 means bank 3
    sec
    sbc r0L
    sta r0H
    lda $dd00           // Read the existing value from $dd00
    and #$fc            // Set just the bank selection bits (0 & 1)
    ora r0H
    sta $dd00
    rts
!:
    ldx #basic.ERROR_ILLEGAL_QUANTITY
    jmp (vectors.IERROR)

/*

    Get the current VIC bank 0-3

*/
CurrentBank:
    lda $dd00
    and #$03
    sta r0L
    lda #$03
    sec
    sbc r0L

    rts

/*

    Set the screen location within the current VIC bank

    Example: SCREEN 1 would set the screen location to offset $0400 of the current VIC bank.
    If in bank 0, this would be $0400 since bank 0 starts at $0000. This is also the default
    at startup.

*/
ScreenCmd:
    jsr CurrentBank     // Get the current VIC bank
    sta r0L
    jsr Get8Bit         // Get the screen number (0-15)
    tya
    pha
    and #$f0            // Strip the bottom 4 bits
    cmp #$00            // Is the value > 0? Means we've specified a number > 15
    bne !+              // Illegal quantity
    pla
    tay
    sty r0H             // Tuck away the screen number (0-15)
    lda $d018           // Set the screen number in $d018
    and #$0f
    ora ScreenLoc, y
    sta $d018
    lda r0H             // Now let BASIC know where that screen is
    asl                 // Multiply by 4
    asl
    sta r0H
    lda r0L
    asl
    asl
    asl
    asl
    asl
    asl
    ora r0H
    sta $288
    rts

!:
    ldx #basic.ERROR_ILLEGAL_QUANTITY
    jmp (vectors.IERROR)

/*

    Execute a PEEK function but return a 16-bit word instead of an 8-bit byte.

    Example: PRINT WEEK(250). Would return the 16-bit value in 250 & 251.

*/
WeekFun:
    jsr basic.GETADR    // Get the WEEK address

    ldy #$00
    lda ($14), y
    sta $62
    iny
    lda ($14), y
    sta $63

    // Thanks to Gregory Naçu for this trick. It allows writing a uint16 to the FAC
    // https://c64os.com/post/floatingpointmath
    ldx #$90
    sec
    jmp $bc49

/*

    Return the start address of the current screen.

    Example: PRINT SCRLOC(0). In the default C64 configuration, this would return 1024.

*/
ScrLocFun:
    lda $288
    sta $62
    lda #$00
    sta $63
    ldx #$90
    sec
    jmp $bc49

/*

    Copy a block of memory.

    Example: MEMCOPY $a000, $a000, $2000 would copy BASIC from ROM to RAM

*/
MemCopyCmd:
    jsr MemCommon
    jsr basic.CHKCOM
    jsr Get16Bit
    lda $14
    sta r2L
    lda $15
    sta r2H

    jsr MemCopy
    rts

/*

    Fill a block of memory with a character

    Example: MEMFILL $0400, $03e8, $20 would fill the screen with space characters.

*/
MemFillCmd:
    jsr MemCommon
    jsr basic.CHKCOM
    jsr Get8Bit
    sty r2L

    jsr MemFill

    rts

/*

    The existing memory routines start with 2 16-bit values and they're written to
    the same registers.

*/
MemCommon:
    jsr Get16Bit
    lda $14
    sta r0L
    lda $15
    sta r0H

    jsr basic.CHKCOM

    jsr Get16Bit
    lda $14
    sta r1L
    lda $15
    sta r1H

    rts

/*

    Fetch a 16 bit value from the current pointer. Value is returned in $14/$15

*/
Get16Bit:
    lda #$00
    sta zp.VALTYP
    jsr basic.FRMNUM
    jsr basic.GETADR
    rts

/*

    Fetch an 8 bit value which is returned in Y

*/
Get8Bit:
    jsr basic.FRMEVL    // Evaluate the expression after the token
    lda zp.VALTYP       // Is it a number?
    cmp #$00
    bne !+              // Nope. Type mismatch
    jsr basic.FACINX    // Convert the value in FAC1 to A(H)&Y(L)
    cmp #$00            // Is the high byte 0? (>255)
    bne !++             // Yup. Illegal quantity

    rts

!:
    ldx #basic.ERROR_TYPE_MISMATCH
    jmp (vectors.IERROR)

!:
    ldx #basic.ERROR_ILLEGAL_QUANTITY
    jmp (vectors.IERROR)

/*

    Common routine to grab some text, ensure it's a number and make sure
    it's < 16. This is used for the Background and Border commands which
    set the colors of each. Returns the value in A

*/
GetColor:
    jsr Get8Bit
    tya
    pha
    and #$f0            // Strip the lower nybble of the low byte. These are really the only bits we care about.
    cmp #$00            // Is the upper nybble > 0? If so, that means our value is > 15 which is not a valid color.
    bne !+              // Illegal quantity
    pla
    rts
!:
    lda #<InvalidColorError     // Write out a custom error message
    sta $22
    lda #>InvalidColorError
    jmp basic.CUSTERROR

/*

    Detokenize. Converts tokens back into PETSCII. This is called when you list a program
    with custom commands. It ensures that those commands expand correctly to their PETSCII
    form.

*/
ConvertFromTokens:
    bpl Out
    bit zp.GARBFL
    bmi Out
    cmp #$ff
    beq Out
    cmp #CMDSTART
    bcs NewList
    jmp $a724
Out:
    jmp $a6f3
NewList:
    sec
    sbc #$cb
    tax
    sty zp.FORPNT
    ldy #-1
Next:
    dex
    beq Found
Loop:
    iny
    lda NewTab, y
    bpl Loop
    bmi Next
Found:
    iny
    lda NewTab, y
    bmi OldEnd
    jsr basic.CHAROUT
    bne Found
OldEnd:
    jmp $a6ef

/*

    Tokenize. Converts PETSCII commands into tokens. This routine is called
    as you enter commands in either immediate mode, or as you enter lines of
    BASIC code.

*/
ConvertToTokens:
    ldx zp.TXTPTR
    ldy #4
    sty zp.GARBFL
NextChar:
    lda BUFFER, x
    bpl Normal
    cmp #$ff
    beq TakChar
    inx
    bne NextChar
Normal:
    cmp #' '
    beq TakChar
    sta zp.ENDCHAR
    cmp #QUOTE
    beq GetChar
    bit zp.GARBFL
    bvs TakChar
    cmp #'?'
    bne Skip
    lda #PRINT
    bne TakChar
Skip:
    cmp #'0'
    bcc Skip1
    cmp #'<'
    bcc TakChar
Skip1:
    sty zp.FBUFPT
    ldy #0
    sty zp.COUNT
    dey
    stx zp.TXTPTR
    dex
CmpLoop:
    iny
    inx
TestNext:
    lda BUFFER, x
    sec
    sbc basic.RESLST, y
    beq CmpLoop
    cmp #$80
    bne NextCmd
    ora zp.COUNT
TakChar1:
    ldy zp.FBUFPT
TakChar:
    inx
    iny
    sta BUFFER-5, y
    cmp #0
    beq End
    sec
    sbc #':'
    beq Skip2
    cmp #DATA-':'
    bne Skip3
Skip2:
    sta zp.GARBFL
Skip3:
    sec
    sbc #REM-':'
    bne NextChar
    sta zp.ENDCHAR
RemLoop:
    lda BUFFER, x
    beq TakChar
    cmp zp.ENDCHAR
    beq TakChar
GetChar:
    iny
    sta BUFFER-5, y
    inx
    bne RemLoop
NextCmd:
    ldx zp.TXTPTR
    inc zp.COUNT
Continue:
    iny
    lda basic.RESLST-1, y
    bpl Continue
    lda basic.RESLST, y
    bne TestNext
    beq NewTok
NotFound:
    lda BUFFER, x
    bpl TakChar1
End:
    sta BUFFER-3, y
    dec zp.TXTPTR+1
    lda #$ff
    sta zp.TXTPTR
    rts
NewTok:
    ldy #0
    lda NewTab, y
    bne NewTest
NewCmp:
    iny
    inx
NewTest:
    lda BUFFER, x
    sec
    sbc NewTab, y
    beq NewCmp
    cmp #$80
    bne NextNew
    ora zp.COUNT
    bne TakChar1
NextNew:
    ldx zp.TXTPTR
    inc zp.COUNT
Cont1:
    iny
    lda NewTab-1,y
    bpl Cont1
    lda NewTab, y
    bne NewTest
    beq NotFound

/*

    Add your commands and functions here. The last byte of each command/function name
    must have $80 added to it. Your commands should come first starting from $cc. You can
    let the execution routine know how to identify commands and functions above in CMDSTART,
    CMDEND, FUNSTART, FUNEND. These are the token numbers for each block of commands/functions.
    Our tokens start at $cc and can go up to $fe ($ff is pi). Both the tokenization and
    detokenization routines use this table, so adding them here will ensure that BASIC
    will recognize them as you enter them and will detokenize them when LISTed.

*/
NewTab:
    .text "BORDE"       // $cc
    .byte 'R' + $80
    .text "BACKGROUN"   // $cd
    .byte 'D' + $80
    .text "WOK"         // $ce
    .byte 'E' + $80
    .text "CL"          // $cf
    .byte 'S' + $80
    .text "MEMCOP"      // $d0
    .byte 'Y' + $80
    .text "MEMFIL"      // $d1
    .byte 'L' + $80
    .text "SCREE"       // $d2
    .byte 'N' + $80
    .text "BAN"         // $d3
    .byte 'K' + $80
    .text "WEE"         // $d4
    .byte 'K' + $80
    .text "SCRLO"       // $d5
    .byte 'C' + $80
    .byte 0

/*

    You can create your own custom error messages as well. Set $22 to the LB of the error message
    and A with the HB of the error message and then call basic.CUSTERROR

*/
InvalidColorError:
    .text "INVALID COLO"
    .byte 'R' + $80

// These are bit patterns that we poke into $d018 to set the screen location. We only care about
// bits 4-7 since bit 0 is unused and bits 1-3 select the char rom location within the VIC bank.
ScreenLoc:
    .byte %00000000, %00010000, %00100000, %00110000, %01000000, %01010000, %01100000, %01110000
    .byte %10000000, %10010000, %10100000, %10110000, %11000000, %11010000, %11100000, %11110000
