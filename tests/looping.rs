use rhai::Engine;

#[test]
fn test_loop() {
    let mut engine = Engine::new();

    assert!(engine
        .eval::<bool>(
            "
			let x = 0;
			let i = 0;

			loop {
				if i < 10 {
					x = x + i;
					i = i + 1;
				}
				else {
					break;
				}
			}

			x == 45
		"
        )
        .unwrap())
}
