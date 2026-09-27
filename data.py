import pandas as pd
from sqlalchemy import create_engine

#Path to CSV file
csv_path = r"/Users/connorpiercy/Documents/GitHub/NFL_Project/nfl-team-statistics.csv"

# Database connection string (Update your user, password, host, port, and database name)
# Format: mysql+pymysql://USER:PASSWORD@HOST:PORT/DATABASE_NAME
db_url = "mysql+pymysql://root:rootpassword@localhost:3306/NFL"

try:
    df = pd.read_csv(csv_path)
    
    engine = create_engine(db_url)
    
    df.to_sql(name="nfl_team_statistics", con=engine, if_exists="fail", index=False)
    
    print("✨ Table created and CSV data imported successfully!")

except Exception as e:
    print(f"❌ Error: {e}")
