# Langton's Ant (Fast)
Langton's ant (https://en.wikipedia.org/wiki/Langton%27s_ant)

Langton's ant is a two-dimensional Turing machine with a very simple set of rules but complex emergent behavior.

Squares on a plane are colored variously either black or white. We arbitrarily identify one square as the "ant". The ant can travel in any of the four cardinal directions at each step it takes. The "ant" moves according to these rules:

    At a white square, turn 90° clockwise, flip the color of the square, move forward one unit
    At a black square, turn 90° counter-clockwise, flip the color of the square, move forward one unit

The ant moves in what appears to be fairly irregular pattern until aroung 10,000 moves when it starts building a 'highway' pattern consisting of 104 repeating moves.

This fast version uses a lookup table to calculate the video memory address of each row.

![Langtons-Ant](/LangtonsAnt.PNG)

