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

}  // namespace gol
