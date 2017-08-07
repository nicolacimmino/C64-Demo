# Units #

## Tick (T) ##

The smallest time unit, a tick is equivalent to 1 frame interrupt call.

## Beat (BT) ##

One beat is 16 TICKS. 

# Tune Elements #

## Instrument Table (IT) ##

Each instrument is a sequence of commands, each command is made of 2 bytes. The high nibble is the command, the lower nibble the first operand,
the second byte the second operand.

CMD2 CMD1 CMD0 OP4 OP3 OP2 OP1 OP0 

WRITE_REG   WR \<REG> \<VAL>

  0 0 1 0 REG3-0 | VAL7-0 
  
WAIT        WA 0 \<TICKS>

  0 1 0 TICKS4-0
  
WAIT_NOTE_OFF WA 1 

  0 1 1 NA4-0

## Instruments Pointers Table (IPT) ##

This is a pointers table to the beginning of the instrument, two bytes per instrument, little endian.

```
IPT BYTE IT0_LO IT0_HI 
    BYTE IT1_LO IT1_HI 
    .....
```

## Track (TRK) ##

Each track is made up of one entry per beat. The entry conntains:

```
TRK BYTE BEAT_NUMBER, FREQ_HI, FREQ_LO, INSTR_NUM, DURATION
    BYTE BEAT_NUMBER, FREQ_HI, FREQ_LO, INSTR_NUM, DURATION
    .....
```
