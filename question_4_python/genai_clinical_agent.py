"""
GenAI Clinical Data Assistant

This script demonstrates a lightweight GenAI-style agent that:
- Understands the ADAE dataset schema
- Translates free-text clinical safety questions into structured filters
- Executes Pandas queries to return subject counts and IDs

The LLM component is mocked to demonstrate the Prompt → Parse → Execute flow,
as permitted by the assessment instructions.
"""
import pandas as pd

# ==============================
# Schema Definition
# ==============================

AE_SCHEMA = {
    "AESEV": "Adverse Event severity or intensity",
    "AETERM": "Adverse Event term",
    "AESOC": "Body system or organ class",
    "USUBJID": "Unique subject identifier"
}

class ClinicalTrialDataAgent:
    """
    GenAI-style clinical data agent that translates free-text questions
    into structured Pandas filters using schema awareness.
    """

    def __init__(self, schema):
        self.schema = schema

    def parse_question_with_llm(self, question: str) -> dict:
        q = question.lower()

        if "severity" in q or "moderate" in q:
            return {"target_column": "AESEV", "filter_value": "MODERATE"}

        if "headache" in q:
            return {"target_column": "AETERM", "filter_value": "HEADACHE"}

        if "cardiac" in q:
            return {"target_column": "AESOC", "filter_value": "CARDIAC DISORDERS"}

        raise ValueError("Question not understood")

    def execute_query(self, df, llm_output):
        column = llm_output["target_column"]
        value = llm_output["filter_value"]

        filtered = df[df[column].str.upper() == value]
        subjects = filtered["USUBJID"].unique().tolist()

        return {
            "unique_subject_count": len(subjects),
            "subject_ids": subjects
        }
# ==============================
# Test Script (3 Example Queries)
# ==============================

if __name__ == "__main__":
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
