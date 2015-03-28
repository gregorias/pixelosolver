Overview
========

This is an application which solves the Pixelo flash game available at
[kongregate](http://www.kongregate.com/games/tamaii/pixelo). Solving means:

  1. Taking a screenshot
  2. Analyzing the screenshot to find board and performing OCR to determine hint
     values
  3. Solving the game itself
  4. Clicking through the solution.

The application is written in Haskell and uses some wxHaskell for GUI as well as
x11 library for simulating mouse clicks. Because of the x11 dependency it may
not work on Windows. I couldn't find any cross-platform library.

Install
=======

Run `runhaskell Setup.hs --help` for installation instruction. Alternatively I
recommend using a cabal sandbox via:

    cabal sandbox --init
    cabal install --only-dependencies
    cabal configure
    cabal build

You can now find the executable in `dist/build/pixelosolver`.

Usage
=====

Run the pixelogame and pixelosolver. Load up a puzzle you want the program to
solve. Position the pixelosolver dialog so that the entire board is visible and
there are no ambigouous white patches around. Click the button and watch the
game become solved after a moment (less than a few second a modern PC).

Program checks for groups of white pixels and for example a highscore board is
sufficient to confuse the computer vision algithm.

Important remarks
=================
* The screenshot analysis relies on some constants which I have chosen by hand
  and work my 1920x1080 resolution. On different setups it is likely the a
  different set of constants will be required. The meaning of constants are
  described in `PixeloSolver.AI.OCR`
* The entire solving algorithm runs in a wx thread which makes the window
  unresponsive for a moment. Normally I would offload the processing to a
  separate thread, but I couldn't find a way to provide wxHaskell's main thread
  to run the GUI parts of the processing (taking a screenshot and simulating
  mouse clicks). In wxPython this would be possible with wxEvents.
