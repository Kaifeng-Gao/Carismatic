from transformers import AutoImageProcessor, AutoModelForImageClassification
import torch
from PIL import Image
import os
import pandas as pd
import numpy as np
from tqdm import tqdm


# Configurations
ROOT_DIR = 'data/images/confirmed_fronts/'
CLASS_INDEX = {
    0: "Angry",
    1: "Disgust",
    2: "Fear",
    3: "Happy",
    4: "Sad",
    5: "Surprise",
    6: "Neutral"
}


# Load Processor and Model
processor = AutoImageProcessor.from_pretrained("trpakov/vit-face-expression")
model = AutoModelForImageClassification.from_pretrained("trpakov/vit-face-expression")
device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
print(f"Using device: {device}")
model.to(device)


# prediction function
def predict_expression(image_path):
    # Open the image
    # image = Image.open(image_path)
    gray_image = Image.open(image_path).convert('L')
    image = gray_image.convert('RGB')

    # Preprocess the image
    inputs = processor(images=image, return_tensors="pt")
    inputs = {name: tensor.to(device) for name, tensor in inputs.items()}
    
    # Run inference
    with torch.no_grad():
        outputs = model(**inputs)

    # Get the predicted class
    predicted_class_idx = outputs.logits.argmax(-1).item()
    predicted_prob = outputs.logits[0].softmax(-1).tolist()
    
    # Get the human-readable label
    label = CLASS_INDEX[predicted_class_idx]
    
    return image, label, predicted_prob


# main
def main():
    # Get total number of files for the progress bar
    total_files = sum([len(files) for r, d, files in os.walk(ROOT_DIR)])

    # Create a tqdm object
    pbar = tqdm(total=total_files, desc="Processing files")

    # Initialize an empty list to store results
    results = []

    for dirpath, dirnames, filenames in os.walk(ROOT_DIR):
        for filename in filenames:
            full_path = os.path.join(dirpath, filename)
            image, label, probs = predict_expression(full_path)
            # Store results
            result = {
                'image_path': full_path,
                'label': label
            }
            # Add probability columns
            for i, prob in enumerate(probs):
                result[f'prob_{CLASS_INDEX[i].lower()}'] = prob
            
            results.append(result)
            pbar.update(1)

    pbar.close()
    # Create DataFrame from results
    df = pd.DataFrame(results)

    # Display the first few rows of the DataFrame
    print(df.head())

    # Optionally, save the DataFrame to a CSV file
    df.to_csv('expression_results.csv', index=False)


if __name__ == "__main__":
    main()