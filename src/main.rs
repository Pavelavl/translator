use eframe::egui;
use std::path::PathBuf;

mod code_generator;
mod lexical_analyzer;
mod models;
mod name_table;
mod reader;
mod syntax_analyzer;

use crate::{code_generator::CodeGenerator, reader::Reader, syntax_analyzer::SyntaxAnalyzer};

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
                            match Reader::read_to_string(path.to_str().unwrap()) {
                                Ok(content) => {
                                    self.input_text = content;
                                    self.log_text
                                        .push_str(&format!("Opened file: {:?}\n", path));
                                }
                                Err(e) => {
                                    self.log_text
                                        .push_str(&format!("Error opening file: {e}\n"));
                                }
                            }
                        }
                        ui.close();
                    }
                    if ui.button("Save").clicked() {
                        if let Some(path) = rfd::FileDialog::new().save_file() {
                            self.output_path = Some(path.clone());
                            if let Err(e) =
                                Reader::write_to_file(path.to_str().unwrap(), &self.output_text)
                            {
                                self.log_text.push_str(&format!("Error saving: {e}\n"));
                            } else {
                                self.log_text.push_str("Output saved successfully.\n");
                            }
                        }
                        ui.close();
                    }
                });

                ui.add_space(8.0);

                if ui.button("Compile").clicked() {
                    if self.input_text.is_empty() {
                        self.log_text.push_str("No input text to compile.\n");
                        return;
                    }

                    self.log_text.push_str("Starting compilation...\n");
                    CodeGenerator::clear();

                    if let Err(e) = Reader::init_with_string(&self.input_text) {
                        self.log_text.push_str(&format!("Reader init error: {e}\n"));
                        return;
                    }

                    let mut analyzer = SyntaxAnalyzer::new(self.input_text.clone());
                    if let Err(e) = Reader::init_with_string(&self.input_text) {
                        self.log_text.push_str(&format!("Reader init error: {e}\n"));
                    } else {
                        analyzer.compile();
                        Reader::close();
                    }

                    self.output_text = CodeGenerator::get_code().join("\n");

                    if !analyzer.errors.is_empty() {
                        self.log_text.push_str("Compilation errors found:\n");
                        for err in analyzer.errors {
                            self.log_text.push_str(&format!(
                                "Line {} Col {}: {}\n",
                                err.line, err.col, err.message
                            ));
                        }
                    } else {
                        self.log_text
                            .push_str("Compilation finished successfully.\n");
                    }

                    Reader::close();
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
                        egui::TextEdit::multiline(&mut self.output_text),
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
                                .lock_focus(true)
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
