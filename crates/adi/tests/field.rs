#[test]
fn string_pair_roundtrip() {
    let out = adi::to_string(&("FOO".to_string(), "BAR".to_string())).unwrap();
    assert_eq!(out, "<FOO:3>BAR");
    let back: (String, String) = adi::from_str(&out).unwrap();
    assert_eq!(back, ("FOO".to_string(), "BAR".to_string()));
}

#[test]
fn u32_pair_roundtrip() {
    let out = adi::to_string(&("FOO".to_string(), 100u32)).unwrap();
    let back: (String, u32) = adi::from_str(&out).unwrap();
    assert_eq!(back, ("FOO".to_string(), 100u32));
}

#[test]
fn i32_pair_roundtrip() {
    let out = adi::to_string(&("FOO".to_string(), 42i32)).unwrap();
    let back: (String, i32) = adi::from_str(&out).unwrap();
    assert_eq!(back, ("FOO".to_string(), 42i32));
}

#[test]
fn f64_pair_roundtrip() {
    let out = adi::to_string(&("FOO".to_string(), 14.5f64)).unwrap();
    let back: (String, f64) = adi::from_str(&out).unwrap();
    assert_eq!(back, ("FOO".to_string(), 14.5f64));
}

#[test]
fn bool_pair_roundtrip() {
    let out = adi::to_string(&("FOO".to_string(), true)).unwrap();
    let back: (String, bool) = adi::from_str(&out).unwrap();
    assert_eq!(back, ("FOO".to_string(), true));
}

#[test]
fn empty_string_pair_roundtrip() {
    let out = adi::to_string(&("FOO".to_string(), String::new())).unwrap();
    assert_eq!(out, "<FOO:0>");
    let back: (String, String) = adi::from_str(&out).unwrap();
    assert_eq!(back, ("FOO".to_string(), String::new()));
}

#[test]
fn unit_pair_roundtrip() {
    let out = adi::to_string(&("FOO".to_string(), ())).unwrap();
    assert_eq!(out, "<FOO:0>");
    let back: (String, ()) = adi::from_str(&out).unwrap();
    assert_eq!(back, ("FOO".to_string(), ()));
}
