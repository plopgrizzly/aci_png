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
extern crate png;

use afi::GraphicBuilder;
pub use afi::{ Graphic, GraphicDecodeErr };

/// Decode PNG data.  On success, returns the png as a `Graphic`.
pub fn decode(png: &[u8]) -> Result<Graphic, GraphicDecodeErr> {
	use png::ColorType::*;

	let decoder = png::Decoder::new(png);
	let (info, mut reader) = decoder.read_info().unwrap();

	let mut buf = vec![0; info.buffer_size()];
	reader.next_frame(&mut buf).unwrap();

	let size = (info.width * info.height) as usize;
	let mut out : Vec<u32> = Vec::with_capacity(size);

	let (color, bit) = reader.output_color_type();

	let graphic = match color {
		RGB => {
			let mut pixel : [u8;4] = [0xFF, 0xFF, 0xFF, 0xFF];

			for i in 0..size {
				pixel[0] = buf[i * 3 + 0];
				pixel[1] = buf[i * 3 + 1];
				pixel[2] = buf[i * 3 + 2];

				out.push(unsafe {::std::mem::transmute(pixel)});
			}

			GraphicBuilder::new().rgba(info.width, info.height, out)
		},
		RGBA => {
			let mut pixel : [u8;4] = [0xFF, 0xFF, 0xFF, 0xFF];

			for i in 0..size {
				pixel[0] = buf[i * 4 + 0];
				pixel[1] = buf[i * 4 + 1];
				pixel[2] = buf[i * 4 + 2];
				pixel[3] = buf[i * 4 + 3];

				out.push(unsafe {::std::mem::transmute(pixel)});
			}

			GraphicBuilder::new().rgba(info.width, info.height, out)
		},
		Grayscale => return Err(GraphicDecodeErr::GrayscaleNYI),
		Indexed => return Err(GraphicDecodeErr::IndexedNYI),
		GrayscaleAlpha => return Err(GraphicDecodeErr::AGrayscaleNYI),
	};

	if bit != png::BitDepth::Eight {
		return Err(GraphicDecodeErr::BitsNYI)
	}

	Ok(graphic)
}
