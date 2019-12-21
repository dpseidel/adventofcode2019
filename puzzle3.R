library(tidyverse)
library(sf)
library(testthat)
library(magrittr)

# puzzle 3
### Testing algorithm
dummy_wires <- list(
  set1 = list(
    "R75,D30,R83,U83,L12,D49,R71,U7,L72",
    "U62,R66,U55,R34,D71,R55,D58,R83"
  ),
  set2 = list(
    "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51",
    "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
  )
)

parse_path <- function(vector) {
  vector %>%
    str_split(",") %>%
    map(~ tibble(raw = .x) %>%
      mutate(
        dir = str_sub(raw, 1, 1),
        units = as.numeric(str_sub(raw, 2)),
        x.coord = 0,
        y.coord = 0,
        pos = 1:n(),
        x_dir = case_when(
          dir == "R" ~ x.coord + units,
          dir == "L" ~ x.coord - units,
          TRUE ~ x.coord
        ),
        y_dir = case_when(
          dir == "U" ~ y.coord + units,
          dir == "D" ~ y.coord - units,
          TRUE ~ x.coord
        ),
        x.coord = cumsum(x_dir),
        y.coord = cumsum(y_dir)
      ))
}

dummy_wire1 <- parse_path(dummy_wires[[1]])[[1]]
dummy_wire2 <- parse_path(dummy_wires[[1]])[[2]]

sf_dummywire1 <- st_as_sf(dummy_wire1, coords = c("x.coord", "y.coord")) %>%
  st_coordinates() %>%
  st_linestring()
sf_dummywire2 <- st_as_sf(dummy_wire2, coords = c("x.coord", "y.coord")) %>%
  st_coordinates() %>%
  st_linestring()

cross1 <- st_intersection(sf_dummywire1, sf_dummywire2) %>%
  tibble(x = .[, 1], y = .[, 2]) %>%
  select(x, y) %>%
  mutate(dist = abs(x) + abs(y)) %>%
  arrange(dist)

expect_equal(cross1$dist[1], 159)


### Real input ====
wires <- read_lines("input3.txt") %>%
  parse_path()

wire1 <- st_as_sf(wires[[1]], coords = c("x.coord", "y.coord")) %>%
  st_coordinates() %>%
  st_linestring()

wire2 <- st_as_sf(wires[[2]], coords = c("x.coord", "y.coord")) %>%
  st_coordinates() %>%
  st_linestring()

# plot(wire1)
# plot(wire2, add = T, col = 2)

intersects <- st_intersection(wire1, wire2) %>%
  tibble(x = .[, 1], y = .[, 2]) %>%
  select(x, y) %>%
  mutate(dist = abs(x) + abs(y))

filter(intersects, dist == min(dist))
# answer = 260

## Part 2 -- counting steps... another brute force solution...
wire1_path <- wires[[1]] %$% pmap_df(
  list(x.coord, y.coord, pos, x_dir, y_dir),
  function(x.coord, y.coord, pos, x_dir, y_dir) {
    start <- c(x.coord - x_dir, y.coord - y_dir)
    end <- c(x.coord, y.coord)

    path <- tibble(x = start[1]:end[1], y = start[2]:end[2])

    path[-nrow(path), ]
  }
) %>%
  mutate(step = 0:(n() - 1))

wire2_path <- wires[[2]] %$% pmap_df(
  list(x.coord, y.coord, pos, x_dir, y_dir),
  function(x.coord, y.coord, pos, x_dir, y_dir) {
    start <- c(x.coord - x_dir, y.coord - y_dir)
    end <- c(x.coord, y.coord)

    path <- tibble(x = start[1]:end[1], y = start[2]:end[2])

    path[-nrow(path), ]
  }
) %>%
  mutate(step = 0:(n() - 1))

intersects %>%
  left_join(wire1_path) %>%
  left_join(wire2_path, by = c("x", "y")) %>%
  mutate(total_steps = step.x + step.y) %>%
  arrange(total_steps)
