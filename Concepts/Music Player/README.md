
# TICK (T)

The smallest time unit, a tick is equivalent to 1 frame interrupt call.

# Instrument Table (IT) #

Each instrument is a sequence of commands, each command is made of 2 bytes. The high nibble is the command, the lower nibble the first operand,
the second byte the second operand.

WRITE_REG   WR <REG> <VAL>
WAIT        WA 0 <TICKS>

# Instruments Pointers Table (IPT) #

This is a pointers table to the beginning of the instrument, two bytes per instrument, little endian.

IT0_LO IT0_HI IT1_LO IT1_HI
