// Pseudo 16-bit registers
// NOTE: These may or may not be completely safe for BASIC and the Kernal. Let me know if they cause problems.
// I tried to target casette related addresses, so if they're going to interfere, it's probably going to be
// with anything using cassette.

.label r0   = $fb
.label r0L  = $fb
.label r0H  = $fc
.label r1   = $fd
.label r1L  = $fd
.label r1H  = $fe
.label r2   = $9e
.label r2L  = $9e
.label r2H  = $9f
.label r3   = $a5
.label r3L  = $a5
.label r3H  = $a6
.label r4   = $be
.label r4L  = $be
.label r4H  = $bf
