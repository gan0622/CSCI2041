open LazeeModules
open StreamModules
open Hwk_06_Modules

module Stream_Lazy = Stream(Lazee_v1)
module Stream_Slow = Stream(Lazee_v2)

module Hwk_06_Lazy = Hwk_06( Stream_Lazy )
module Hwk_06_Slow = Hwk_06( Stream_Slow )

module Hwk_07_Test (Hwk_06: Hwk_06_Sig) = struct
  let () =
    assert (Hwk_06.take 4 (Hwk_06.nats) = [1; 2; 3; 4]);
    assert (Hwk_06.take 10 (Hwk_06.nats) = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]);
    assert (Hwk_06.take 4 (Hwk_06.cubes_from 1) = [1; 8; 27; 64]);
    assert (Hwk_06.take 4 (Hwk_06.cubes_from_zip 1) = [1; 8; 27; 64]);
    assert (Hwk_06.take 4 (Hwk_06.cubes_from_map 1) = [1; 8; 27; 64]);

    assert (Hwk_06.take 5 Hwk_06.facts = [1; 1; 2; 6; 24]);
    assert (Hwk_06.take 5 Hwk_06.facts' = [1; 1; 2; 6; 24]);

    assert (Hwk_06.take 10 Hwk_06.primes = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29])
end

module Test_Lazy = Hwk_07_Test(Hwk_06_Lazy)
module Test_Slow = Hwk_07_Test(Hwk_06_Slow)

let () =
  print_endline ("Success, all tests passed.")