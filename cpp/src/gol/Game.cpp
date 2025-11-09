#include "Game.hpp"

#include <ncurses.h>
#include <csignal>
#include <random>

namespace {

void HandleSigint(int) {
  gol::Game::CleanupRender();
  std::_Exit(0);
}

}  // namespace

namespace gol {

Game::Game(int width, int height)
    : width_(width),
      height_(height),
      grid_(height, std::vector<bool>(width, false)) {}

void Game::Seed(std::optional<int> seed) {
  int _seed = seed.value_or(std::rand());
  std::mt19937 gen(_seed);
  std::bernoulli_distribution dist(0.5);

  for (int y = 0; y < height_; ++y) {
    for (int x = 0; x < width_; ++x) {
      grid_[y][x] = dist(gen);
    }
  }
}

void Game::InitRender() {
  initscr();
  cbreak();
  noecho();

  std::signal(SIGINT, HandleSigint);
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
