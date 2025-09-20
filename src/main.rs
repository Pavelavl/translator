use eframe::egui::{self};
use std::path::PathBuf;

mod compiler;
mod code_generator;
mod lexical_analyzer;
mod models;
mod name_table;
mod reader;
mod writer;
mod syntax_analyzer;

use crate::{compiler::Compiler, reader::Reader, writer::Writer};

struct TranslatorApp {
    input_path: Option<PathBuf>,
    output_path: Option<PathBuf>,
    input_text: String,
    output_text: String,
    log_text: String,
}

impl TranslatorApp {
    fn new(_cc: &eframe::CreationContext<'_>) -> Self {
        TranslatorApp {
            input_path: None,
            output_path: None,
            input_text: String::new(),
            output_text: String::new(),
            log_text: String::new(),
        }
    }
}

impl eframe::App for TranslatorApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::TopBottomPanel::top("menu").show(ctx, |ui| {
            ui.horizontal(|ui| {
                ui.menu_button("File", |ui| {
                    if ui.button("Open").clicked() {
                        if let Some(path) = rfd::FileDialog::new().pick_file() {
                            self.input_path = Some(path.clone());
                            if let Some(path_str) = path.to_str() {
                                match Reader::read_to_string(path_str) {
                                    Ok(content) => {
                                        self.input_text = content;
                                        self.log_text.push_str(&format!("Opened file: {:?}\n", path));
                                    }
                                    Err(e) => {
                                        self.log_text.push_str(&format!("Error opening file: {e}\n"));
                                    }
                                }
                            } else {
                                self.log_text.push_str("Error: Invalid file path encoding\n");
                            }
                            ui.close();
                        }
                    }
                    if ui.button("Save").clicked() {
                        let w = Writer::new();
                        if let Some(path) = rfd::FileDialog::new().save_file() {
                            self.output_path = Some(path.clone());
                            if let Some(path_str) = path.to_str() {
                                if let Err(e) = w.write_to_file(path_str, &self.output_text) {
                                    self.log_text.push_str(&format!("Error saving: {e}\n"));
                                } else {
                                    self.log_text.push_str("Output saved successfully.\n");
                                }
                            } else {
                                self.log_text.push_str("Error: Invalid file path encoding\n");
                            }
                            ui.close();
                        }
                    }
                });

                ui.add_space(8.0);

                if ui.button("Compile").clicked() {
                    if self.input_text.is_empty() {
                        self.log_text.push_str("No input text to compile.\n");
                        return;
                    }

                    self.log_text.push_str("Starting compilation...\n");
                    let mut compiler = Compiler::new(&self.input_text);

                    match compiler.compile() {
                        Ok(asm_code) => {
                            let w = Writer::new();
                            self.output_text = asm_code;
                            self.log_text.push_str("Compilation finished successfully.\n");
                            let output_file = self.output_path.as_ref().map_or("test.asm", |p| p.to_str().unwrap_or("test.asm"));
                            if let Err(e) = w.write_to_file(output_file, &self.output_text) {
                                self.log_text.push_str(&format!("Failed to write {}: {e}\n", output_file));
                            } else {
                                self.log_text.push_str(&format!("Generated {} successfully.\n", output_file));
                                match std::process::Command::new("tasm").arg(output_file).output() {
                                    Ok(output) => {
                                        self.log_text.push_str(&format!(
                                            "TASM Output:\n{}",
                                            String::from_utf8_lossy(&output.stdout)
                                        ));
                                        if !output.status.success() {
                                            self.log_text.push_str(&format!(
                                                "TASM Errors:\n{}",
                                                String::from_utf8_lossy(&output.stderr)
                                            ));
                                        }
                                    }
                                    Err(e) => {
                                        self.log_text.push_str(&format!(
                                            "Failed to run TASM: {e}. Ensure TASM is installed and in your system PATH.\n"
                                        ));
                                    }
                                }
                            }
                        }
                        Err(errors) => {
                            self.output_text.clear();
                            self.log_text.push_str("Compilation errors found:\n");
                            for err in errors {
                                self.log_text.push_str(&format!(
                                    "Line {} Col {}: {}\n",
                                    err.line, err.col, err.message
                                ));
                            }
                        }
                    }
                }
            });
        });

        egui::CentralPanel::default().show(ctx, |ui| {
            let half_width = ui.available_width() / 2.0;
            ui.horizontal(|ui| {
                ui.vertical(|ui| {
                    ui.set_max_width(half_width);
                    ui.heading("Input");
                    ui.add_sized(
                        [half_width, ui.available_height()],
                        egui::TextEdit::multiline(&mut self.input_text),
                    );
                });
                ui.separator();
                ui.vertical(|ui| {
                    ui.set_max_width(half_width);
                    ui.heading("Output");
                    ui.add_sized(
                        [half_width, ui.available_height()],
                        egui::TextEdit::multiline(&mut self.output_text).interactive(true),
                    );
                });
            });
        });

        egui::TopBottomPanel::bottom("log_block")
            .resizable(true)
            .default_height(150.0)
            .show(ctx, |ui| {
                ui.horizontal(|ui| {
                    ui.heading("Log");
                    if ui.button("Clear Log").clicked() {
                        self.log_text.clear();
                    }
                });
                ui.separator();
                let max_log_height = 200.0;
                egui::ScrollArea::vertical()
                    .max_height(max_log_height)
                    .show(ui, |ui| {
                        ui.add_sized(
                            [ui.available_width(), ui.available_height()],
                            egui::TextEdit::multiline(&mut self.log_text)
                                .desired_rows(6)
                                .interactive(true)
                                .frame(true),
                        );
                    });
            });
    }
}

fn main() -> Result<(), eframe::Error> {
    let options = eframe::NativeOptions::default();
    eframe::run_native(
        "Translator",
        options,
        Box::new(|cc| Ok(Box::new(TranslatorApp::new(cc)))),
    )
}