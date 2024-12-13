# Sorting Algorithm Visualization in Haskell

This project visualizes the sorting process of various algorithms using the functional programming language **Haskell**. It features a **graphical user interface (GUI)** that allows users to select sorting algorithms through clickable buttons. The sorting process is illustrated using bars of different colors to indicate comparisons and operations.

## Features

- **Sorting Algorithms**:
  - Bubble Sort
  - Insertion Sort
  - Selection Sort
  - Merge Sort
  - Quick Sort
- **Interactive GUI**:
  - Choose sorting algorithms via buttons on a control panel.
  - Start, pause and replay the sorting visualization with dedicated buttons.
- **Real-Time Visualization**:
  - Colored bars represent elements being sorted.
  - Dynamic highlights indicate the elements currently being compared or moved.

## Prerequisites

To set up and run this project, ensure you have the following installed on your system:

1. **Haskell Compiler**: [GHC](https://www.haskell.org/ghc/) - The Glasgow Haskell Compiler.
2. **Cabal**: [Cabal](https://www.haskell.org/cabal/) - A build tool for Haskell projects.
3. **Gloss**: A Haskell library for creating 2D graphics. You can install it using:
   ```bash
   cabal install --lib gloss

## Setup Instructions

1. **Clone the repository**
  ```bash
  git clone https://github.com/FatSijarina/AlgoVisualization.git
  cd AlgorithmVisualization
  cd app
  ```
2. **Run the code**
   ```bash
   runghc main.hs

## How to use

1. Launch the application. A window will open with the visualization and a set of buttons at the bottom.
2. **Select a Sorting Algorithm**: Click one of the algorithm buttons (Bubble, Selection, Insertion, Merge, Quick) to choose a sorting algorithm.
3. **Play/Pause**:
    - Click the Play button to start the visualization.
    - Click the Pause button to pause the visualization at any time.
4. **Observe the bars as they dynamically represent the sorting process**:
    - Colors indicate which elements are being compared or swapped.

## Project Structure

The project follows a modular structure for clarity and maintainability:

- **`Common.hs`**: Defines shared data structures, such as `SortStep`, to represent the state of the sorting process.
- **`Algorithms.hs`**: Combines various sorting algorithms implemented in their own modules:
  - `Algorithms.BubbleSort`
  - `Algorithms.InsertionSort`
  - `Algorithms.SelectionSort`
  - `Algorithms.MergeSort`
  - `Algorithms.QuickSort`
- **`Visualization.hs`**: Handles drawing the array, buttons, and other visual components.
- **`Controller.hs`**: Manages the application's state, including event handling and updates for the sorting process.
- **`Main.hs`**: The entry point for the application. Sets up the window and connects the controller with the visualization.

## Technologies used

  - **Haskell**: Functional programming language used to implement the project.
  - **Gloss**: Library for rendering 2D graphics.
