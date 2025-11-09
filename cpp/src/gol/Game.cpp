#include "Game.hpp"

#include <ncurses.h>
#include <random>

namespace gol {

Game::Game(int width, int height)
    : width_(width),
      height_(height),
      grid_(height, std::vector<bool>(width, false)) {}

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
  initscr();
  cbreak();
  noecho();
};

void Game::Render() {
  for (int y = 0; y < height_; ++y) {
    for (int x = 0; x < width_; ++x) {
      const char cell = grid_[y][x] ? '*' : ' ';
      mvaddch(y, x, cell);
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
