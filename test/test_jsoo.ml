module Runner = Test_shared.Make (Transit_jsoo.Transit.Json)

let () =
  Runner.run_fixed ();
  Runner.run_random_output "jsoo"
