import sqlite3


def parsePeriod(period: int):
    if period == 0:
        return "AllYear"
    elif period == 1:
        return "First"
    elif period == 2:
        return "Second"


def main():
    # Connect to the SQLite database
    conn = sqlite3.connect('../ispchecker.db')
    conn.row_factory = sqlite3.Row
    cursor = conn.cursor()

    # Query to select all entries from the 'course' table
    cursor.execute("SELECT * FROM course")

    # Fetch all rows from the executed query
    courses = cursor.fetchall()

    # Open a file in write mode
    with open('output_code_file.isp', 'w+') as file:
        # Iterate over each course entry
        for course in courses:
            # Assuming 'course' table has columns: id, name, description
            name = course['name']
            code = course['code']
            period = parsePeriod(course['period'])
            desc = course['description']
            studyPoints = course['study_points']

            # Add some code to the file for each entry
            file.write('Course {\n')
            file.write(f'\tname: "{name}",\n')
            file.write(f'\tcode: {code},\n')
            if desc:
                file.write(f'\tdescription: "{desc}",\n')
            file.write(f'\tperiod: {period},\n')
            file.write(f'\tstudyPoints: {studyPoints},\n')
            file.write('}\n')
            file.write('\n')

    # Close the connection to the database
    conn.close()

    print("Code has been written to output_code_file.txt")


if __name__ == '__main__':
    main()
