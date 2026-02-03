import pandas as pd
from genai_clinical_agent import ClinicalTrialDataAgent, AE_SCHEMA

# Load ADAE data (ensure adae.csv is in this folder or update the path)
ae = pd.read_csv("adae.csv")

agent = ClinicalTrialDataAgent(AE_SCHEMA)

test_questions = [
    "Give me the subjects who had adverse events of Moderate severity",
    "Show subjects with headache adverse events",
    "How many subjects had cardiac adverse events"
]

for q in test_questions:
    print("=" * 60)
    print("Question:", q)
    parsed = agent.parse_question_with_llm(q)
    result = agent.execute_query(ae, parsed)
    print(result)
