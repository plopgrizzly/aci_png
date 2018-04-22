// lib.rs -- Aldaron's Codec Interface / PNG
// Copyright (c) 2017-2018  Jeron A. Lau <jeron.lau@plopgrizzly.com>
// Licensed under the MIT LICENSE

//! Aldaron's Codec Interface / PNG is a library developed by Plop Grizzly to
//! encode(TODO) and decode png and apng(TODO) image files.

#![warn(missing_docs)]
#![doc(html_logo_url = "http://plopgrizzly.com/icon.png",
	html_favicon_url = "http://plopgrizzly.com/icon.png",
	html_root_url = "http://plopgrizzly.com/aci_png/")]

extern crate afi;

mod png;

use afi::GraphicBuilder;
pub use afi::{ Graphic, GraphicDecodeErr };

/// Encode PNG data.  Set `alpha` to `true` if you want to save the alpha
/// channel.
pub fn encode(mut graphic: Graphic, alpha: bool) -> Vec<u8> {
	let mut out = vec![];
	graphic.rgba();
	let graphic = graphic.as_bytes();

	png::write(&mut out, graphic.0, graphic.1, graphic.2, alpha).unwrap();

	out
}

/// Decode PNG data.  On success, returns the png as a `Graphic`.
pub fn decode(png: &[u8]) -> Result<Graphic, GraphicDecodeErr> {
	let mut reader = ::std::io::Cursor::new(png);
	let i: Result<png::Image<u8>, png::Error> = png::read(&mut reader);
	let image = if let Result::Ok(p) = i { p }
		else { return Err(GraphicDecodeErr::IncorrectFormat) };
	let bytes: Vec<u8> = image.buf;

	let size = (image.w * image.h) as usize;
	let mut out: Vec<u32> = Vec::with_capacity(size);
	let mut pixel: u32 = 0xFFFFFFFF;
	let rgba: &mut [u8] = unsafe {
		::std::slice::from_raw_parts_mut(&mut pixel as *mut _
			as *mut u8, 4)
	};

	for i in 0..size {
		rgba[0] = bytes[i * 4 + 0];
		rgba[1] = bytes[i * 4 + 1];
		rgba[2] = bytes[i * 4 + 2];
		rgba[3] = bytes[i * 4 + 3];

		out.push(pixel);
	}

	Ok(GraphicBuilder::new().rgba(image.w, image.h, out))
}
