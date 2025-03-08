# Tri-render

Tri-render is a 3D rendering engine written in 6502 assembly for the Nintendo Entertainment System.
It is currently a work in progress, and many core features are in active development.
In particular, `main.asm` is in a state of flux while the code in `render.asm` is being tested.
That being said, the main branch has a functional point-cloud renderer that renders a model's vertices as sprites on the screen.

# Controls

The control system is currently relative to a global reference frame, as opposed to being relative to the orientation of the camera.

- Up/down moves along the Z-axis.
- Left/right moves along the X-axis.
- A/B moves along the Y-axis.
- Holding select while inputting the above allows you to instead rotate about a given axis.

# Building

This project depends only on make, python3, and the cc65 compiler suite. Compiling should be as simple as running `make` in the root directory of the project.
There exists an additional make target, `make mesen`, which will automatically load the resulting `.nes` file into the Mesen emulator, if it is available in your path.