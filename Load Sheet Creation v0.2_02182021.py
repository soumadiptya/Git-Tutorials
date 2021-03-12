# -*- coding: utf-8 -*-
"""
Created on Thu Jan 28 12:29:15 2021

@author: Soumadiptya.c
"""
#%% Import Libraries
import numpy as np
import pandas as pd
import re

#%% Helper Functions
def extract_freq_unit(time_string):
    pattern_to_extract = '[a-z]{1,}'
    freq_unit = re.findall(pattern_to_extract, time_string)
    if len(freq_unit) == 0:
        freq_unit = ""
    else:
        freq_unit = freq_unit[0]
    # Consolidate different freq units
    if freq_unit in ['years', 'year', 'y']:
        freq_unit = 'years'
    elif freq_unit in ['months', 'month', 'm']:
        freq_unit = 'months'
    elif freq_unit in ['days', 'day', 'd']:
        freq_unit = 'days'
    elif freq_unit in ['weeks', 'week', 'w']:
        freq_unit = 'weeks'
    elif freq_unit in ['per']:
        freq_unit = 'per iow'
    return freq_unit

def get_freq(time_string):
    pattern_to_sub = "[a-z]"
    freq = re.sub(pattern_to_sub, "", time_string)
    freq = re.sub('\s+', '', freq)
    return freq

#%% Main
if __name__ == "__main__":
    # Ingest data
    load_sheet_template_motors = pd.read_excel("Work Program Loader Sheet 1-8-2021.xlsx")
    motor_standards = \
    pd.read_excel("./Standards/Motors-Phase II2020-09-10_22-23_updated.xlsx",
                  sheet_name="Motors-Phase II.xlsx")
    # Fill columns
    # For Motors
    motor_standards.fillna("", inplace=True)
    motor_standards['Maintenance Standard Description'] = \
    motor_standards['File_name'] + '_' + \
    motor_standards['Sheet'] + '_' + \
    motor_standards['Fleet_Team'] + '_' + \
    motor_standards['Content_owner'] + '_' + \
    motor_standards['Revision'] + '_' + \
    motor_standards['Revision_date'] + '_' + \
    motor_standards['Component_Classification'] + '_' + \
    motor_standards['Component_Description'] + '_' + \
    motor_standards['Component_specific_classification'] + '_' + \
    motor_standards['Component_Specific_description'] + '_' + \
    motor_standards['Applicable_Sister_units'] + '_' + \
    motor_standards['Maintenance_Activity_comments'] + '_' + \
    motor_standards['Frequency Type']
    motor_standards['Maintenance Standard ID'] = motor_standards.groupby('Maintenance Standard Description').ngroup()
    motor_standards['Maintenance Standard ID'] = motor_standards['Maintenance Standard ID'] + 1
    motor_standards['Maintenance Standard ID'] = "MTR_" + motor_standards['Maintenance Standard ID'].astype(str)
    # Fill for Motors
    load_sheet_template_motors['Maintenance Standard ID'] = motor_standards['Maintenance Standard ID']
    load_sheet_template_motors['Maintenance Standard Description'] = motor_standards['Maintenance Standard Description']
    load_sheet_template_motors['Maintenance Task Description'] = motor_standards['Maintenance Task Name']
    load_sheet_template_motors['Time Frequency'] = motor_standards['Frequency']
    load_sheet_template_motors['Time Units']    = motor_standards['Frequency Type']
    load_sheet_template_motors['Maintenance Task ID'] = \
    load_sheet_template_motors.groupby(['Maintenance Standard Description'])['Maintenance Task Description'].cumcount()
    load_sheet_template_motors['Maintenance Task ID'] = load_sheet_template_motors['Maintenance Task ID'] + 1
    load_sheet_template_motors['Maintenance Task ID'] = load_sheet_template_motors['Maintenance Standard ID'] + '_' + load_sheet_template_motors['Maintenance Task ID'].astype(str)
    load_sheet_template_motors['Operating_Conditions'] = motor_standards['Operating_Conditions']
    load_sheet_template_motors['Time Frequency'] = load_sheet_template_motors['Time Frequency'].map(lambda x:str(x).lower())
    load_sheet_template_motors['Time Units alternative'] = load_sheet_template_motors['Time Frequency'].map(extract_freq_unit)
    load_sheet_template_motors['Time Frequency alternative'] = load_sheet_template_motors['Time Frequency'].map(get_freq)
    load_sheet_template_motors.drop(columns = ['Time Frequency', 'Time Units'], inplace=True)
    load_sheet_template_motors.rename(columns={'Time Frequency alternative':'Time Frequency', 'Time Units alternative':'Time Units'}, inplace=True)
    # Sort data by Descriptions
    load_sheet_template_motors.sort_values("Maintenance Standard Description", inplace=True)
    # Write data
    # load_sheet_template_motors.to_excel('Load_sheet_template_filled_motor_step5.xlsx', index=False)