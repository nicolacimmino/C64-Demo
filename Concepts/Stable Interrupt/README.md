# The issue

Many things we want to do on the screen are time critical as they are achieved by changing VIC registers on the fly while 
the screen is being drawn. Say, for instance, we want a white bar across the screen from lines 90 to 100. We will need to
have our program know exactly when we are at the beginning of line 90, change the border and backgound color to white and,
when we reach line 100, switch back the colors to normal.

The VIC has a feature, the raster interrupt, that is capable of generating an interrupt when it reaches a certain raster line.
The problem with interrupts is that the time elaspsed from the moment the interrupt is raised and when your code actually runs
can vary. This depends on the instruction that is executing which needs to terminate before the interrupt routine is called.
Since different instructions have different lengths the call to the ISR has some jitter (up to 7 cycles).

This means that if you manipulate any register that affects the sceen this will happen at slightly different times in each frame and,
as a consequence, will jitter on the screen.

# The solution

The most common solution makes use of a double interrupt. The first interrupt is used to setup a second one and, once ready, the 
routine executes a series of NOP (2 cycles long) instead of returning from interrupt which would allow again other code, with
instructions on various lengths, to execute. In this way the seconnd interrupt happens with a jitter of just 1 cycle (because it
will surely happen during a NOP, 2 cycles, so it will be delayed by either 1 or 2 cycles).

To get rit of the last cycle of jitter the second ISR then executes instructions that take exactly one line (63 cycles on PAL) in such a 
way that a comparison of the line number with the actual current line number ($D012) happens exactly either at the cycle before a new
line begins or just after it has begun. In this way it's possible to know if we have an offset of -1 from the start of the line
(comparison shows we are still on the original interrupt line) or 0 from the start of the line (comparison shows we are on the next line).
With this information at hand we can now delay, if needed, by one cycle the execution and get rid of the last bit of jitter.

What is interesting to note is that even though the shortest instruction is 2 cycles long, and comparisions are longer, we are still able 
to detect a 1 cycle shift and introduce a 1 cycle delay when needed. The next section will detail how this seemingly impossible feat is
carried out.

