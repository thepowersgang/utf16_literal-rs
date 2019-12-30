//! This crate provides the utf16! macro, that takes a string literal and produces
//! a &[u16; N] containing the UTF-16 encoded version of that string.
//!
//! ```
//! #![feature(proc_macro_hygiene)]	// Needed to use the macro in an expression
//! extern crate utf16_literal;
//!
//! # fn main() {
//! let v = utf16_literal::utf16!("Foo\u{1234}");
//! assert_eq!(v[0], 'F' as u16);
//! assert_eq!(v[1], 'o' as u16);
//! assert_eq!(v[2], 'o' as u16);
//! assert_eq!(v[3], 0x1234);
//! # }
//! ```

extern crate proc_macro;

use proc_macro::TokenStream;

#[proc_macro]
/// Emit a UTF-16 encoded string as a `&[u16; N]`
pub fn utf16(input: TokenStream) -> TokenStream
{
	let mut it = input.into_iter();

	let mut rv = Vec::new();
	loop
	{
		match it.next()
		{
		Some(::proc_macro::TokenTree::Literal(l)) => {
			let s = match literal_to_string(l)
				{
				Ok(s) => s,
				Err(l) => panic!("Unexpected token '{}'", l),
				};
			for c in s.chars()
			{
				if c as u32 <= 0xFFFF {
					rv.push(::proc_macro::TokenTree::Literal(::proc_macro::Literal::u16_suffixed(c as u32 as u16)));
					rv.push(::proc_macro::TokenTree::Punct(::proc_macro::Punct::new(',', ::proc_macro::Spacing::Alone)));
				}
				else {
					let v = c as u32 - 0x1_0000;
					let hi = v >> 10;
					assert!(hi <= 0x3FF);
					let lo = v & 0x3FF;

					rv.push(::proc_macro::TokenTree::Literal(::proc_macro::Literal::u16_suffixed(0xD800 + hi as u16)));
					rv.push(::proc_macro::TokenTree::Punct(::proc_macro::Punct::new(',', ::proc_macro::Spacing::Alone)));
					rv.push(::proc_macro::TokenTree::Literal(::proc_macro::Literal::u16_suffixed(0xDC00 + lo as u16)));
					rv.push(::proc_macro::TokenTree::Literal(::proc_macro::Literal::u16_suffixed(c as u32 as u16)));
					rv.push(::proc_macro::TokenTree::Punct(::proc_macro::Punct::new(',', ::proc_macro::Spacing::Alone)));
				}
			}
			},
		Some(t) => panic!("Unexpected token '{}'", t),
		None => panic!("utf16! requires a string literal argument"),
		}


		match it.next()
		{
		Some(::proc_macro::TokenTree::Punct(ref v)) if v.as_char() == ',' => {},
		Some(t) => panic!("Unexpected token '{}'", t),
		None => break,
		}
	}

	// Create the borrowed array
	vec![
		::proc_macro::TokenTree::Punct( ::proc_macro::Punct::new('&', ::proc_macro::Spacing::Alone) ),
		::proc_macro::TokenTree::Group( ::proc_macro::Group::new(::proc_macro::Delimiter::Bracket, rv.into_iter().collect()) ),
		].into_iter().collect()
}

fn literal_to_string(lit: ::proc_macro::Literal) -> Result<String,::proc_macro::Literal>
{
	let formatted = lit.to_string();
	
	let mut it = formatted.chars();
	if it.next() != Some('"') {
		return Err(lit);
	}

	let mut rv = String::new();
	loop
	{
		match it.next()
		{
		Some('"') =>
			match it.next()
			{
			Some(v) => panic!("malformed string, stray \" in the middle (followed by '{:?}')", v),
			None => break,
			},
		Some('\\') =>
			match it.next()
			{
			Some('x') => {
				let d1 = it.next().expect("malformed string, \\x with EOS").to_digit(16).expect("maformed string, \\x followed by non-hex");
				let d2 = it.next().expect("malformed string, \\x with EOS").to_digit(16).expect("maformed string, \\x followed by non-hex");
				let v = (d1 << 16) | d2;
				rv.push(v as u8 as char);
				},
			Some('u') => {
				assert_eq!(it.next(), Some('{'), "malformed string, \\u with no brace");
				let mut c = it.next().expect("malformed string, \\u with EOS");
				let mut ch = 0;
				while let Some(v) = c.to_digit(16)
				{
					ch *= 16;
					ch |= v;
					c = it.next().expect("malformed string, \\u with EOS");
				}
				assert_eq!(c, '}', "malformed string, \\u with no closing brace");
				rv.push(::std::char::from_u32(ch).expect("malformed string, \\u with invalid scalar value"));
				},
			Some('0') => rv.push('\0'),
			Some('\\') => rv.push('\\'),
			Some('\"') => rv.push('\"'),
			Some('r') => rv.push('\r'),
			Some('n') => rv.push('\n'),
			Some('t') => rv.push('\t'),
			Some(c) => panic!("TODO: Escape sequence \\{:?}", c),
			None => panic!("malformed string, unexpected EOS (after \\)"),
			},
		Some(c) => rv.push(c),
		None => panic!("malformed string, unexpected EOS"),
		}
	}

	Ok(rv)
}

