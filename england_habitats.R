pacman::p_load(fastverse)

hab_tbl <- fread('data/Living_England_Habitat_Map_Phase4_1377467643975416716.csv', nrows = 1000)

hab_tbl

collapse::qsu(hab_tbl)
