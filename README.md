# HalfHeight

An extension to `UI.NCurses` for drawing half-height console graphics.

By drawing a Unicode [Upper Half Block](https://www.compart.com/en/unicode/U+2580) character (▀) with different foreground and background colors, one can simulate drawing two rows of a 2D grid of colors in one row, achieving grid cells that take up half a console row alongside regular console text.

![](example.png)

There's a [brief writeup](https://askham.ai/2020/11/26/half-height-console-graphics.html) on my blog.

The main caveat is that one is now restricted to 15 unique colors: `UI.NCurses` usually lets you define 255 custom foreground/background combinations, but now we might have any two colors appear as our foreground/background.

## Usage Example

The above Mandelbrot example can be seen in `app/Main.hs`. To use:

- Register `Color 1` through `Color 15` by providing a list of 15 hex strings to `initHexColors`. This sets the terminal colors appropriately, registers a unique curses `ColorID` for every possible foreground/background combination of each of the 15 colors, and returns a map from (fg, bg) tuples to corresponding `ColorID`.
- Create a `Buffer` (a 2D `Vector` of `Color`) by providing dimensions and a default color to `mkBuffer`.
- Draw on this buffer using `SetXY`.
- When ready to blit to the screen, `drawBuffer` will create the appropriate curses `Update ()` operations for writing the corresponding Upper Half Block characters to the terminal to render the buffer rows at half-height.
