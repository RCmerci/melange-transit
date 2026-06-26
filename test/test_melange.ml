module Runner = Test_shared.Make (Transit_melange.Transit.Json)

let () =
  Runner.run_fixed ();
  Runner.run_random_output "melange"
