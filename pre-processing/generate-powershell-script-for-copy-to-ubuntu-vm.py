import os
import io

curent_directory = os.getcwd()
files_of_current_directory = [f for f in os.listdir(curent_directory) if os.path.isfile(os.path.join(curent_directory, f))]

commands = []
format_string = 'Copy-VMFile -Name Ubuntu2004 -SourcePath \'{}\' -DestinationPath \'{}\' -FileSource Host'
for file_of_current_directory in files_of_current_directory:
    full_path_name = os.path.join(curent_directory, file_of_current_directory)
    commands.append(format_string.format(full_path_name, '/home/tzouvanas/thesis/fq'))

f = open("copy-files-to-ubuntu-vm.ps1", "w")
f.writelines(commands)
f.close()