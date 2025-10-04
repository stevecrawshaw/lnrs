import streamlit as st
import duckdb
import os
import pandas as pd

def get_env_vars():
    env_vars = {}
    with open(".env") as f:
        for line in f:
            if line.strip() and not line.startswith("#"):
                key, value = line.strip().split("=", 1)
                env_vars[key.strip()] = value.strip().strip('"')
    return env_vars

env_vars = get_env_vars()

motherduck_token = env_vars.get("motherduck_token")

def get_db_connection():
    return duckdb.connect(f"md:lnrs_weca?motherduck_token={motherduck_token}")

st.title("LNRS Database CRUD")

con = get_db_connection()
tables = con.execute("SHOW TABLES").fetchall()
table_names = [table[0] for table in tables]

selected_table = st.selectbox("Select a table", table_names)

if selected_table:
    st.write(f"### Data from {selected_table}")
    
    # READ
    data = con.execute(f"SELECT * FROM {selected_table} LIMIT 100").fetchdf()
    st.write(data)
    st.dataframe(data)

    # DELETE
    st.write("### Delete a record")
    if not data.empty:
        col1, col2 = st.columns([1, 3])
        with col1:
            record_to_delete = st.number_input("Enter ID to delete", min_value=0, step=1)
        with col2:
            st.write("")
            st.write("")
            if st.button("Delete record"):
                try:
                    primary_key_column = data.columns[0]
                    con.execute(f"DELETE FROM {selected_table} WHERE {primary_key_column} = ?", (record_to_delete,))
                    st.success(f"Record with ID {record_to_delete} deleted successfully.")
                    # Refresh data
                    data = con.execute(f"SELECT * FROM {selected_table} LIMIT 100").fetchdf()
                    st.experimental_rerun()
                except Exception as e:
                    st.error(f"Error deleting record: {e}")
    else:
        st.write("No data to delete.")

    # CREATE
    st.write("### Create a new record")
    
    columns = con.execute(f"DESCRIBE {selected_table}").fetchall()
    column_names = [col[0] for col in columns]
    
    new_record_values = {}
    for col_name in column_names:
        new_record_values[col_name] = st.text_input(f"Enter value for {col_name}")

    if st.button("Create record"):
        try:
            placeholders = ", ".join(["?" for _ in column_names])
            values = tuple(new_record_values.values())
            con.execute(f"INSERT INTO {selected_table} ({', '.join(column_names)}) VALUES ({placeholders})", values)
            st.success("Record created successfully.")
            # Refresh data
            data = con.execute(f"SELECT * FROM {selected_table} LIMIT 100").fetchdf()
            st.experimental_rerun()
        except Exception as e:
            st.error(f"Error creating record: {e}")

    # UPDATE
    st.write("### Update a record")
    if not data.empty:
        record_to_update = st.number_input("Enter ID to update", min_value=0, step=1, key="update_id")
        
        update_record_values = {}
        for col_name in column_names:
            update_record_values[col_name] = st.text_input(f"Enter new value for {col_name}", key=f"update_{col_name}")

        if st.button("Update record"):
            try:
                primary_key_column = data.columns[0]
                set_clause = ", ".join([f"{col} = ?" for col in column_names])
                values = list(update_record_values.values())
                values.append(record_to_update)
                con.execute(f"UPDATE {selected_table} SET {set_clause} WHERE {primary_key_column} = ?", tuple(values))
                st.success(f"Record with ID {record_to_update} updated successfully.")
                # Refresh data
                data = con.execute(f"SELECT * FROM {selected_table} LIMIT 100").fetchdf()
                st.experimental_rerun()
            except Exception as e:
                st.error(f"Error updating record: {e}")
    else:
        st.write("No data to update.")

con.close()
