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

/// Decode PNG data.  On success, returns image as vector:
/// `[width, height, first BGRA pixel .. last BGRA pixel]`
pub fn decode(png: &'static [u8]) -> Result<Vec<u32>, DecodeErrorPNG> {
	use png::ColorType::*;

	let decoder = png::Decoder::new(png);
	let (info, mut reader) = decoder.read_info().unwrap();

	let mut buf = vec![0; info.buffer_size()];
	reader.next_frame(&mut buf).unwrap();

	let size = (info.width * info.height) as usize;
	let mut out = Vec::with_capacity(size + 2);

	unsafe {
		out.set_len(size + 2);
	}

	out[0] = info.width;
	out[1] = info.height;

	let (color, bit) = reader.output_color_type();

	match color {
		RGB => {
			let mut pixel: [u8; 4] = unsafe {
				::std::mem::uninitialized()
			};

			for i in 0..size {
				pixel[0] = buf[i * 3 + 2];
				pixel[1] = buf[i * 3 + 1];
				pixel[2] = buf[i * 3 + 0];
				pixel[3] = 0xFF;

				out[2 + i] = unsafe {
					::std::mem::transmute(pixel)
				};
			}
		},
		RGBA => {
			let mut pixel: [u8; 4] = unsafe {
				::std::mem::uninitialized()
			};

			for i in 0..size {
				pixel[0] = buf[i * 4 + 2];
				pixel[1] = buf[i * 4 + 1];
				pixel[2] = buf[i * 4 + 0];
				pixel[3] = buf[i * 4 + 3];

				out[2 + i] = unsafe {
					::std::mem::transmute(pixel)
				};
			}
		},
		Grayscale => return Err(DecodeErrorPNG::GrayscaleNYI),
		Indexed => return Err(DecodeErrorPNG::IndexedNYI),
		GrayscaleAlpha => return Err(DecodeErrorPNG::AGrayscaleNYI),
	}

	if bit != png::BitDepth::Eight {
		return Err(DecodeErrorPNG::BitsNYI)
	}

	Ok(out)
}
