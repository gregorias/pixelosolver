# PixeloSolver

![example video](doc/ExampleVideo/PixeloVideo.gif)

Overview
--------

This application solves the Pixelo flash game available at
[kongregate](http://www.kongregate.com/games/tamaii/pixelo). Solving means:

  1. Taking a screenshot
  2. Analyzing the screenshot to find board and performing OCR to determine hint
     values. If a digit can't be recognized the application will output its
     black and white bitmap and ask for suggestion.
  3. Solving the game itself
  4. Clicking through the solution.

It is written in Haskell and uses some wxHaskell for GUI as well as x11 library
for simulating mouse clicks. Because of the x11 dependency it may not work on
Windows, but it is easy to write functions for it if availability on the Windows
system is necessary.

Install
-------

Run `runhaskell Setup.hs --help` for installation instruction. Alternatively I
recommend using a cabal sandbox via:

    cabal sandbox init
    cabal install --only-dependencies
    cabal configure
    cabal build

You can now find the executable in `dist/build/pixelosolver/`.

Usage
-----

Run the pixelogame and pixelosolver. Load up a puzzle you want the program to
solve. Position the pixelosolver dialog so that the entire board is visible and
there are no ambiguous white patches around. Click the button and watch the
game become solved after a moment (less than a few second a modern PC).

Program checks for groups of white pixels and for example a high-score board is
sufficient to confuse the computer vision algorithm.

Example
-------

There is an example function which loads an image stored in a file and outputs
it to the console. This function is called `pipelineStatic` in `Main` and could
be run with attached example screenshot in `resources/PixeloScreenshot.png`. To
run it just type:

    cabal repl
    runExceptT $ pipelineStatic "resources/PixeloScreenshot.png"

You do not need to run the game to see the program working.

Remarks
-------
* The screenshot analysis relies on some constants which I have chosen by hand
  and work my 1920x1080 resolution. On different setups it is likely the a
  different set of constants will be required. The meaning of constants is
  described in `PixeloSolver.AI.OCR and the constants themselves are in the
  Main.

* The entire solving algorithm runs in a wx thread which makes the window
  unresponsive for a moment. Normally I would offload the processing to a
  separate thread, but I couldn't find a way to provide wxHaskell's main thread
  to run the GUI parts of the processing (taking a screenshot and simulating
  mouse clicks). In wxPython this would be possible with wxEvents.

* On 1920x1080 resolution the program requires less than 1kB of stack, but
  memory profiling shows that the program uses around 140MB heap space (SYSTEM
  is the retainer). If I run the program without the GUI it uses around 6MB of
  heap space (exactly the size of the screenshot at 1920x1080 resolution).
  Unfortunately I can't decouple the OCR thread from wx Main thread, as
  described above, so the problem stays until it is solved.
