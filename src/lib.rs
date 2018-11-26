// Copyright Jeron Lau 2017 - 2018.
// Dual-licensed under either the MIT License or the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at https://www.boost.org/LICENSE_1_0.txt)

//! Aldaron's Codec Interface / PNG is a library developed by Plop Grizzly to
//! encode and decode png and apng(TODO) image files.

#![warn(missing_docs)]
#![doc(html_logo_url = "http://plopgrizzly.com/icon.png",
	html_favicon_url = "http://plopgrizzly.com/icon.png",
	html_root_url = "http://plopgrizzly.com/aci_png/")]

extern crate afi;
extern crate byteorder;

mod png;

use std::io::{ Read, Seek, Cursor };
use afi::*;

/// Encoder for PNG/APNG.
pub struct PngEncoder {
	// Image size.
	wh: (u16, u16),
	// 3(false) or 4(true) channels.
	format: ColorChannels,
}

impl EncoderV for PngEncoder {
	fn new(video: &Video) -> PngEncoder {
		let wh = video.wh();
		let format = video.format();

		PngEncoder {
			wh, format
		}
	}

	fn run(&mut self, frame: &VFrame) -> Vec<u8> {
		let alpha = self.format.n_channels() == 4;
		let mut out = vec![];
		// TODO: Use frame.sample_rgba
		png::write(&mut out, self.wh.0, self.wh.1, &frame.0, alpha)
			.unwrap();
		out
	}

	fn end(self) -> Vec<u8> {
		vec![]
	}
}

/// Decoder for PNG/APNG.
pub struct PngDecoder<T: Read + Seek> {
	#[allow(unused)] // TODO
	data: T,
	channels: ColorChannels,
	wh: (u16, u16),
	n_frames: u32,
	out: Vec<u8>, // TODO: Don't store here.
}

impl<T> Decoder<T> for PngDecoder<T> where T: Read + Seek {
	fn new(mut data: T, channels: afi::ColorChannels)
		-> Option<PngDecoder<T>>
	{
		let n_frames = 1;

		let i: Result<png::Image<u8>, png::Error> = {
			png::read(&mut data)
		};
		let image = if let Result::Ok(p) = i { p }
			else { return None };
		let bytes: Vec<u8> = image.buf;

		let size = image.w as usize * image.h as usize;
		let mut out: Vec<u8> = Vec::with_capacity(size
			* channels.n_channels());

		for i in 0..size {
			let rgba = channels.from(Srgba,
				[bytes[i*4+0], bytes[i*4+1],
				 bytes[i*4+2], bytes[i*4+3]]);

			for i in 0..channels.n_channels() {
				out.push(rgba[i]);
			}
		}

		let wh = (image.w, image.h);

		// TODO: Return None if it's not a PNG
		Some(PngDecoder { data, channels, wh, n_frames, out })
	}

	fn run(&mut self, audio: &mut Option<Audio>, video: &mut Option<Video>)
		-> Option<bool>
	{
		if audio.is_none() && video.is_none() {
			*video = Some(Video::new(
				self.channels, self.wh, self.n_frames
			));

			// First run, initialize structs, some left.
			Some(true)
		} else {
			// Non-first run, decode 1 frame.
			let video = video.as_mut().unwrap(); // Isn't none, so fine.
			video.add(VFrame(self.out.clone())); // TODO: unnecessary clone

			// TODO, return True if APNG and has more frames.
			Some(false)
		}
	}

	fn get(&self) -> Index {
		// TODO
		Index(0)
	}

	fn set(&mut self, _index: Index) {
		// TODO
	}
}

/// Simple API for quickly loading files.
pub fn decode(file: &[u8], channels: ColorChannels) -> Option<Video> {
	let mut image = None;
	let mut decoder = PngDecoder::new(Cursor::new(file), channels)?;
	decoder.run(&mut None, &mut image).unwrap();
	decoder.run(&mut None, &mut image).unwrap();
	image
}
