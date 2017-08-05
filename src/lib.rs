extern crate png;

/// The errors that can be returned if `decode()` fails.
pub enum DecodeErrorPNG {
	/// Not a PNG file (bad header)
	NotPNG,
	/// Dimensions are not numbers
	BadNum,
	/// Not yet implemented
	GrayscaleNYI,
	/// Not yet implemented
	RgbNYI,
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
			DecodeErrorPNG::RgbNYI => "NYI: RGB",
			DecodeErrorPNG::IndexedNYI => "NYI: Indexed",
			DecodeErrorPNG::AGrayscaleNYI => "NYI: AGrayscale",
			DecodeErrorPNG::BitsNYI => "NYI: bad bits",
		})
	}
}

/// Decode PNG data.  On success, returns image as tuple:
/// `(width, height, pixels)`
pub fn decode(png: &'static [u8])
	-> Result<(u32, u32, Vec<u8>), DecodeErrorPNG>
{
	let decoder = png::Decoder::new(png);
	let (info, mut reader) = decoder.read_info().unwrap();

	let mut buf = vec![0; info.buffer_size()];
	reader.next_frame(&mut buf).unwrap();

	let (color, bit) = reader.output_color_type();

	use png::ColorType::*;

	match color {
		Grayscale => return Err(DecodeErrorPNG::GrayscaleNYI),
		RGB => return Err(DecodeErrorPNG::RgbNYI),
		Indexed => return Err(DecodeErrorPNG::IndexedNYI),
		GrayscaleAlpha => return Err(DecodeErrorPNG::AGrayscaleNYI),
		RGBA => {},
	}

	if bit != png::BitDepth::Eight {
		return Err(DecodeErrorPNG::BitsNYI)
	}

	Ok((info.width, info.height, buf))
}
