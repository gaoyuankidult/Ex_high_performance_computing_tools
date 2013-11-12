solution:

#a) measure the CPU time as a function of for being either single or double precision.
Comment your results.

Answer:

The result is stored in the file "plot_difference_single_double.png". It seems there is not much differences between single and double precision. The reason is that If the floating point is done via the "legacy" floating point unit then double precision will incur no speed penalty. Due to the native precision of the floating point unit, the double precision may even be faster.




