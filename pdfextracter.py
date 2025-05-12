import fitz  # PyMuPDF
import json
import pytesseract  # For OCR
from pdf2image import convert_from_path  # Convert PDF pages to images
import re
import os
import sys

# Load mappings.json
with open("mappings.json", "r", encoding="utf-8") as f:
    term_mappings = json.load(f)


def extract_thresholds_from_pdf(pdf_path, output_file="thresholds.pl", icd_file="icd_dataset.json", mappings_file="mappings.json"):
    """
    Extract thresholds from a PDF with OCR fallback for scanned PDFs.
    """
    try:
        # Load ICD-11 dataset
        with open(icd_file, "r", encoding="utf-8") as f:
            icd_data = json.load(f)

        # Load mappings.json
        with open(mappings_file, "r", encoding="utf-8") as f:
            term_mappings = json.load(f)

        if not os.path.exists(pdf_path):
            raise FileNotFoundError(f"PDF file not found: {pdf_path}")

        doc = fitz.open(pdf_path)
        extracted_thresholds = []
        unmapped_entries = []
        duplicate_entries = set()

        for page_num in range(len(doc)):
            page = doc[page_num]
            text = page.get_text("text", flags=fitz.TEXT_PRESERVE_LIGATURES)

            # If no text, use OCR
            if not text.strip():
                print(f"Page {page_num + 1} contains no extractable text. Using OCR...")
                images = convert_from_path(pdf_path, first_page=page_num + 1, last_page=page_num + 1)
                text = pytesseract.image_to_string(images[0], lang="est")

            # Process extracted text using both icd_data and term_mappings
            new_thresholds, unmapped = extract_thresholds_from_text(text, icd_data, term_mappings)
            extracted_thresholds += new_thresholds
            unmapped_entries += unmapped

        # Handle duplicates
        unique_thresholds = []
        for threshold in extracted_thresholds:
            if threshold in duplicate_entries:
                continue
            elif threshold in unique_thresholds:
                duplicate_entries.add(threshold)
            else:
                unique_thresholds.append(threshold)

        # Save extracted thresholds
        with open(output_file, "w", encoding="utf-8") as f:
            for threshold in unique_thresholds:
                f.write(threshold + ".\n")
        print(f"Thresholds saved to {output_file}")

        # Save duplicates and unmapped entries
        if duplicate_entries:
            with open("duplicates.log", "w", encoding="utf-8") as f:
                f.write("\n".join(duplicate_entries))
            print(f"Duplicates logged in 'duplicates.log'.")
        if unmapped_entries:
            with open("unmapped_entries.log", "w", encoding="utf-8") as f:
                f.write("\n".join(unmapped_entries))
            print(f"Unmapped entries logged in 'unmapped_entries.log'.")

    except Exception as e:
        print(f"Error processing PDF: {e}")



def extract_thresholds_from_text(text, icd_data, term_mappings):
    """
    Extract thresholds from text, associating each term with its correct ICD code and description.
    """
    extracted_thresholds = []
    unmapped_entries = []

    for line in text.splitlines():
        mapped = False

        # 1. Attempt to map using mappings.json
        for term, standardized_name in term_mappings.items():
            if term.lower() in line.lower():
                value = extract_numeric_value(line)
                if value is not None:
                    # Find the matching ICD code and description for this measurement
                    for code, details in icd_data.items():
                        if standardized_name in details.get("thresholds", {}):
                            description = details["description"]
                            threshold = f"icd_threshold('{code}', '{description}', '{standardized_name}', {value})"
                            extracted_thresholds.append(threshold)
                            mapped = True
                            break
                if mapped:
                    break
        # 2. Log unmapped lines
        if not mapped:
            unmapped_entries.append(line)
    return extracted_thresholds, unmapped_entries



def extract_numeric_value(line):
    """
    Extract numeric value from a line of text.
    """
    match = re.search(r"(\d+(\.\d+)?|\d+,\d+)", line)  # Support both dot and comma as decimal separators
    if match:
        value = match.group(1).replace(",", ".")  # Convert comma to dot for float compatibility
        return float(value)
    return None

if __name__ == "__main__":
    if len(sys.argv) > 1:
        pdf_path = sys.argv[1]  # Get PDF path from the first command-line argument
        extract_thresholds_from_pdf(pdf_path, "thresholds.pl", "icd_dataset.json")  # Specify output file and ICD dataset
    else:
        print("No PDF path provided. Usage: python pdfextracter.py <path_to_pdf>")
