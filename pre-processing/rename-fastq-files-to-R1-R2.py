import os
import io

curent_directory = os.getcwd()
files_of_current_directory = [f for f in os.listdir(curent_directory) if os.path.isfile(os.path.join(curent_directory, f))]

for file_of_current_directory in files_of_current_directory:
    if file_of_current_directory.endswith('.fastq'):
        full_path_name = os.path.join(curent_directory, file_of_current_directory)
        if '@R' in full_path_name:
            os.rename(full_path_name, full_path_name.replace('@R', '_R2'))
        if '@F' in full_path_name:
            os.rename(full_path_name, full_path_name.replace('@F', '_R1'))
