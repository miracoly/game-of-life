#include "Game.hpp"

#include <ncurses.h>
#include <cmath>
#include <random>
#include <stdexcept>
#include <string>

namespace gol {

Game::Game(int width, int height)
    : width_(width),
      height_(height),
      grid_(height, std::vector<Cell>(width, (Cell){.isAlive = false})) {
  if (height & 1) throw std::invalid_argument("Height must be even");
}

std::uint32_t Game::Seed(std::optional<std::uint32_t> seed) {
  const std::uint32_t actual_seed = seed.value_or(std::random_device{}());
  std::mt19937 gen(actual_seed);
  std::bernoulli_distribution dist(0.5);

  for (int y = 0; y < height_; ++y) {
    for (int x = 0; x < width_; ++x) {
      grid_[y][x] = (Cell){.isAlive = dist(gen)};
    }
  }

  return actual_seed;
}

void Game::Step() {
  std::vector<std::vector<Cell>> newGrid(height_, std::vector<Cell>(width_));
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

  initHexColor(COLOR_FG_0, "#b34b5b");
  initHexColor(COLOR_FG_1, "#db9197");
  initHexColor(COLOR_BG, "#3b161d");
  init_pair(1, COLOR_FG_0, COLOR_BG);
  init_pair(2, COLOR_FG_1, COLOR_BG);
  init_pair(3, COLOR_FG_0, COLOR_FG_1);
  init_pair(4, COLOR_FG_1, COLOR_FG_0);
};

void Game::Render() {
  for (int y = 0; y < height_; y += 2) {
    for (int x = 0; x < width_; ++x) {
      const Cell top = grid_[y][x];
      const Cell bottom = grid_[y + 1][x];
      int _y = y / 2;

      cchar_t cell;
      if (top.isAlive && bottom.isAlive) {
        const wchar_t* c = top.age == bottom.age ? CELL_TOP_BOTTOM : CELL_TOP;
        const short color = top.age >= 1 ? 2 : 1;
        setcchar(&cell, c, 0, color, nullptr);
      } else if (top.isAlive) {
        short color = top.age >= 1 ? 2 : 1;
        setcchar(&cell, CELL_TOP, 0, color, nullptr);
      } else if (bottom.isAlive) {
        short color = bottom.age >= 1 ? 2 : 1;
        setcchar(&cell, CELL_BOTTOM, 0, color, nullptr);
      } else {
        setcchar(&cell, NO_CELL, 0, 1, nullptr);
      }

      mvadd_wch(_y, x, &cell);
    }
  }
  refresh();
};

void Game::CleanupRender() {
  endwin();
}

Cell Game::nextCellState(int x, int y) {
  const int neighbors = countNeighbors(x, y);
  const Cell curr = grid_[y][x];
  if (curr.isAlive) {
    const bool alive = neighbors == 2 || neighbors == 3;
    return (Cell){.isAlive = alive, .age = alive ? curr.age + 1 : -1};
  } else {
    const bool alive = neighbors == 3;
    return (Cell){.isAlive = alive, .age = alive ? 0 : -1};
  }
}

int Game::countNeighbors(int x, int y) {
  int count = 0;
  for (auto neighbor : Game::neighborOffsets) {
    int nx = x + neighbor.first;
    int ny = y + neighbor.second;

    if (inBounds(nx, ny) && grid_[ny][nx].isAlive) ++count;
  }
  return count;
}

bool Game::inBounds(int x, int y) {
  return x > 0 && y > 0 && x < width_ && y < height_;
}

void Game::initHexColor(short colorId, const std::string& hex) {
  if (hex.size() != 7 || hex[0] != '#')
    throw std::invalid_argument("Invalid hex color format");

  short r = colorFromHex(hex.substr(1, 2));
  short g = colorFromHex(hex.substr(3, 2));
  short b = colorFromHex(hex.substr(5, 2));
  init_color(colorId, r, g, b);
}

short Game::colorFromHex(const std::string& hex) {
  return static_cast<short>(
      std::round((std::stoi(hex, nullptr, 16) / 255.0) * 1000));
}

}  // namespace gol
