# Pente - Lisp

This project is a part of the CMPS 366, [Organization of Programming Languages](https://pages.ramapo.edu/~amruth/teaching/opl/f23/opl.html) (referred colloquially with a mixture of dread, resentment, and apprehension as "OPL"), course during Fall 2023 at Ramapo College of New Jersey. The same game is implemented in four different programming languages that are based on different paradigms: C++ (structured and object-oriented), Lisp (functional), Java/Android (object-oriented and event-driven for the GUI), and Prolog (logic). This is the Lisp implementation of the game.

## Implementation in Other Paradigms

### [Structured & OOP - C++](https://github.com/bibhubhatta/cmps366-pente-cpp)

### [OOP & Event-Driven - Java/Android](https://github.com/bibhubhatta/cmps366-pente-java)

### [Logic - Prolog](https://github.com/bibhubhatta/cmps366-pente-prolog)

## Pente

Pente is a two-player board game that is played on a 19x19 board. The game is played with black and white stones. The objective of the game is to place five stones in a row, either horizontally, vertically, or diagonally. The game is played in turns, with each player placing one stone on the board per turn. The game ends when one player has five stones in a row, or when the board is full and no player has won. The player with five stones in a row wins the game.

## Requirements of the Project

### Objective

Win by placing at least five stones in an uninterrupted line or capturing five pairs of opponent's stones while scoring as many points as possible.

### Players

One human and one computer player, playing a tournament consisting of multiple rounds.

### Setup

19x19 board with intersections labeled by columns (A-S) and rows (1-19).

### First Player

Determined by a coin toss in the first round and by points in subsequent rounds.

### Turns

The first player places a white stone at the center (J10) on the first turn and another white stone at least 3 intersections away on the second turn. Players alternate turns thereafter.

### Capturing Stones

A player can capture a pair of opponent's stones if they place their stones on both sides of the opponent's stones in a row, column, or diagonal.

### End of Round

A round ends when a player places five stones in a row or captures five pairs of opponent's stones.

### Scoring

Points are awarded for placing five stones in a row, capturing pairs of opponent's stones, and placing four stones in a row.

### Tournament

The human player can choose to play another round or end the tournament. The winner is the player with the most points.

### Computer Strategy

The computer must have strategies for creating and countering initiatives.

### User Interface

Provide a user-friendly interface with ASCII graphics and command-line input. Validate all human inputs and display the computer's moves and strategies.

### Help Mode

The computer must provide recommendations for the human player's moves upon request.

### Serialization

Allow the game to be saved and resumed later by saving the current state to a text file.

More information of the project requirements can be found at [Professor Amruth Kumar's website](https://pages.ramapo.edu/~amruth/teaching/opl/projects/pente/problem.html).

## More Information

As part of the project, a report was submitted alongside the code. Some of the relevant sections are added below:

### Extra Features

The board can be resized into any length and width, even rectangles. All game logic and IO operations are compatible with different sizes.

### Description of Data Structures

#### Board

A list of list of symbols – used to represent the board as defined in the serialization files

#### Game State

A list containing the board, scores, captures and the next player as defined in the serialization field

#### Position

A string representing the position in the board. Many functions are implemented to support coordinate geometry point operations such as finding neighbors, rows, columns and diagonals

#### List

All aggregate data types are list. The memoization code copied from fare-memoization uses maps for memoization, but I did not write that.

## How to run the program

"pente.lisp" is the entry point of the program.

The code should be compatible with any Common Lisp implementation. The code was run tested with Steel Bank Common Lisp (SBCL) during development using the following command:

```bash
sbcl --script pente.lisp
```

Ensure that the current working directory is the "src" directory.

### Screenshots

#### First player of the round being determined

![First player of the round being determined](docs/media/image4.png)

#### Computer’s move being explained

![Computer’s move being explained](docs/media/image2.png)

#### Computer providing help

![Computer providing help](docs/media/image3.png)

#### Winner of the round being announced

![Winner of the round being announced](docs/media/image10.png)

#### Winner of the tournament being announced

![Winner of the tournament being announced](docs/media/image1.png)

## Function Call Chart

I tried parsing the lisp syntax into a tree and then converting it into a chart, but the resulting image was too big and too cluttered. I spent an unhealthy amount of time beautifying it, hoping if I fine-tuned some parameters just right, it would fall into place. But it didn't. So, I drew the charts in the nick of time, and they are not as neat as I would have liked.

![](docs/media/image11.jpg)
![](docs/media/image21.jpg)
![](docs/media/image9.jpg)
![](docs/media/image16.jpg)
![](docs/media/image17.jpg)
![](docs/media/image8.jpg)
![](docs/media/image5.jpg)
![](docs/media/image24.jpg)
![](docs/media/image15.jpg)
![](docs/media/image13.jpg)
![](docs/media/image14.jpg)
![](docs/media/image19.jpg)
![](docs/media/image22.jpg)
![](docs/media/image20.jpg)
![](docs/media/image6.jpg)
![](docs/media/image18.jpg)
![](docs/media/image7.jpg)
![](docs/media/image12.jpg)
![](docs/media/image23.jpg)
