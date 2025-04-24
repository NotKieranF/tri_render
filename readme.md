# Tri-render

Tri-render is a 3D rendering engine written in 6502 assembly for the Nintendo Entertainment System.
It is currently a work in progress, and many core features are in active development.
That being said, the main branch has a functional polygon renderer that renders a flat shaded polygon to the screen.

# Controls

- Up/down/left/right moves the current vertex.
- A changes the selection to the next vertex.
- B changes the selection to the previous vertex.
- Start makes the polygon one step darker.
- Select makes the polygon one step brighter.
- Holding select while inputting the above allows you to instead rotate about a given axis.

# Building

This project depends only on make, python3, and the cc65 compiler suite. Compiling should be as simple as running `make` in the root directory of the project.
There exists an additional make target, `make mesen`, which will automatically load the resulting `.nes` file into the Mesen emulator, if it is available in your path.