// Aldaron's Codec Interface / PNG
// Copyright (c) 2017 Plop Grizzly, Jeron Lau <jeron.lau@plopgrizzly.com>
// Licensed under the MIT LICENSE
//
// src/lib.rs

//! Aldaron's Codec Interface / PNG is a library developed by Plop Grizzly for
//! decoding png image files.

#![warn(missing_docs)]
#![doc(html_logo_url = "http://plopgrizzly.com/icon.png",
	html_favicon_url = "http://plopgrizzly.com/icon.png",
	html_root_url = "http://plopgrizzly.com/aci_png/")]

extern crate ami;
extern crate afi;
extern crate png;

/// The errors that can be returned if `decode()` fails.
#[derive(Debug)]
pub enum DecodeErrorPNG {
	/// Not a PNG file (bad header)
	NotPNG,
	/// Dimensions are not numbers
	BadNum,
	/// Not yet implemented
	GrayscaleNYI,
	/// Not yet implemented
	IndexedNYI,
	/// Not yet implemented
	AGrayscaleNYI,
	/// Bits NYI
	BitsNYI,
}

impl ::std::fmt::Display for DecodeErrorPNG {
	fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
		write!(f, "Couldn't parse PNG because: {}", match *self {
			DecodeErrorPNG::NotPNG => "Not a PNG file (bad header)",
			DecodeErrorPNG::BadNum => "Dimensions are not numbers",
			DecodeErrorPNG::GrayscaleNYI => "NYI: Grayscale",
			DecodeErrorPNG::IndexedNYI => "NYI: Indexed",
			DecodeErrorPNG::AGrayscaleNYI => "NYI: AGrayscale",
			DecodeErrorPNG::BitsNYI => "NYI: bad bits",
		})
	}
}

/// Decode PNG data.  On success, returns the png as a `Graphic`.
pub fn decode(png: &'static [u8]) -> Result<afi::Graphic, DecodeErrorPNG> {
	use png::ColorType::*;

	let decoder = png::Decoder::new(png);
	let (info, mut reader) = decoder.read_info().unwrap();

	let mut buf = vec![0; info.buffer_size()];
	reader.next_frame(&mut buf).unwrap();

	let size = (info.width * info.height) as usize;
	let mut out : ami::Vec<u32> = ami::Vec::with_capacity(size + 2);

	out.push(info.width);
	out.push(info.height);

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

			afi::GraphicBuilder::new().rgba(out)
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

			afi::GraphicBuilder::new().rgba(out)
		},
		Grayscale => return Err(DecodeErrorPNG::GrayscaleNYI),
		Indexed => return Err(DecodeErrorPNG::IndexedNYI),
		GrayscaleAlpha => return Err(DecodeErrorPNG::AGrayscaleNYI),
	};

	if bit != png::BitDepth::Eight {
		return Err(DecodeErrorPNG::BitsNYI)
	}

	Ok(graphic)
}
