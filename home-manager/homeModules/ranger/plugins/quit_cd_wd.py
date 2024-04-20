import ranger.api
from ranger.api.commands import *
import os

def save_wd(command):
    with open(os.path.expanduser('~/.ranger_quit_cd_wd'), 'w') as f:
        f.write(command.fm.thisdir.path);

class quit_cd_wd(Command):
    """:chdir to working directory of ranger after quiting on ranger.

    """
    def _exit_no_work(self):
        if self.fm.loader.has_work():
            self.fm.notify('Not quitting: Tasks in progress: Use `quit!` to force quit')
        else:
            self.fm.exit()

    def execute(self):
        if len(self.fm.tabs) >= 2:
            self.fm.tab_close()
        else:
            save_wd(self)
            self._exit_no_work()

class quitall_cd_wd(Command):
    """:chdir to working directory of ranger after quitalling on ranger.

    """
    def _exit_no_work(self):
        if self.fm.loader.has_work():
            self.fm.notify('Not quitting: Tasks in progress: Use `quitall!` to force quit')
        else:
            self.fm.exit()

    def execute(self):
        save_wd(self)
        self._exit_no_work()
