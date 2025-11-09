#include "Game.hpp"

#include <ncurses.h>
#include <random>
#include <stdexcept>

namespace gol {

Game::Game(int width, int height)
    : width_(width),
      height_(height),
      grid_(height, std::vector<bool>(width, false)) {
  if (height & 1) throw std::invalid_argument("Height must be even");
}

std::uint32_t Game::Seed(std::optional<std::uint32_t> seed) {
  const std::uint32_t actual_seed = seed.value_or(std::random_device{}());
  std::mt19937 gen(actual_seed);
  std::bernoulli_distribution dist(0.5);

  for (int y = 0; y < height_; ++y) {
    for (int x = 0; x < width_; ++x) {
      grid_[y][x] = dist(gen);
    }
  }

  return actual_seed;
}

void Game::Step() {
  std::vector<std::vector<bool>> newGrid(height_, std::vector<bool>(width_));
  for (int y = 0; y < height_; ++y) {
    for (int x = 0; x < width_; ++x) {
      newGrid[y][x] = nextCellState(x, y);
    }
  }
  grid_ = std::move(newGrid);
}

void Game::InitRender() {
  setlocale(LC_ALL, "");
  initscr();
  cbreak();
  noecho();
  start_color();
  use_default_colors();

  init_pair(1, COLOR_FG_0, COLOR_FG_0);
  init_pair(2, COLOR_FG_0, COLOR_BG);
  init_pair(3, COLOR_BG, COLOR_FG_0);
  init_pair(4, COLOR_BG, COLOR_BG);
};

void Game::Render() {
  for (int y = 0; y < height_; y += 2) {
    for (int x = 0; x < width_; ++x) {
      const bool top = grid_[y][x];
      const bool bottom = grid_[y + 1][x];
      int _y = y / 2;

      cchar_t cell;
      if (top && bottom) {
        setcchar(&cell, CELL_TOP_BOTTOM, COLOR_PAIR(1), 0, nullptr);
      } else if (top) {
        setcchar(&cell, CELL_TOP, COLOR_PAIR(1), 0, nullptr);
      } else if (bottom) {
        setcchar(&cell, CELL_BOTTOM, COLOR_PAIR(1), 0, nullptr);
      } else {
        setcchar(&cell, NO_CELL, COLOR_PAIR(1), 0, nullptr);
      }

      mvadd_wch(_y, x, &cell);
    }
  }
  refresh();
};

void Game::CleanupRender() {
  endwin();
}

bool Game::nextCellState(int x, int y) {
  const int neighbors = countNeighbors(x, y);
  if (grid_[y][x]) {
    return neighbors == 2 || neighbors == 3;
  } else {
    return neighbors == 3;
  }
}

int Game::countNeighbors(int x, int y) {
  int count = 0;
  for (auto neighbor : Game::neighborOffsets) {
    int nx = x + neighbor.first;
    int ny = y + neighbor.second;

    if (inBounds(nx, ny) && grid_[ny][nx]) ++count;
  }
  return count;
}

bool Game::inBounds(int x, int y) {
  return x > 0 && y > 0 && x < width_ && y < height_;
}

}  // namespace gol
