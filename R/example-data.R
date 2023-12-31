example_df_a <- read.csv(text = "
           car,  mpg, cyl,  disp,  hp, drat,    wt, vs, am
    Duster 360, 14.3,   8, 360.0, 245, 3.21, 3.570,  0,  0
 Mazda RX4 Wag, 21.0,   6, 160.0, 110, 3.90, 2.875,  0,  1
      Merc 230, 22.8,   4, 140.8,  95, 3.92, 3.150,  1,  0
    Datsun 710, 22.8,  NA, 109.0,  93, 3.85, 2.320,  1,  1
     Merc 240D, 24.4,   4, 146.7,  62, 3.69, 3.190,  1,  0
Hornet 4 Drive, 21.4,   6, 259.0, 110, 3.08, 3.215,  1,  0
     Mazda RX4, 21.0,   6, 160.0, 110, 3.90, 2.620,  0,  1
       Valiant, 18.1,   6, 225.0, 105, 2.76, 3.460,  1,  0
      Merc 280, 19.2,   6, 167.6, 123, 3.92, 3.440,  1,  0
", stringsAsFactors = FALSE, strip.white = TRUE) %>%
  as_tibble()

example_df_b <- read.csv(text = "
            car,    wt,  mpg,  hp, cyl,  disp, carb, drat, vs
      Merc 240D, 3.190, 26.4,  62,   4, 146.7,    2, 3.69,  1
        Valiant, 3.460, 18.1, 105,   6, 225.0,    1, 2.76,  1
     Duster 360, 3.570, 16.3, 245,   8, 360.0,    4, 3.21,  0
     Datsun 710, 2.320, 22.8,  93,  NA, 108.0,    1, 3.85,  1
      Merc 280C, 3.440, 17.8, 123,   6, 167.6,    4, 3.92,  1
       Merc 280, 3.440, 19.2, 123,   6, 167.6,    4, 3.92,  1
 Hornet 4 Drive, 3.215, 21.4, 110,   6, 258.0,    1, 3.08,  1
     Merc 450SE, 4.070, 16.4, 180,   8, 275.8,    3, 3.07,  0
       Merc 230, 3.150, 22.8,  95,   4, 140.8,    2, 3.92,  1
  Mazda RX4 Wag, 2.875, 21.0, 110,   6, 160.0,    4, 3.90,  0
", stringsAsFactors = FALSE, strip.white = TRUE) %>%
  as_tibble()
