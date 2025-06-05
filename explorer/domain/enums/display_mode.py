# domain/explorer/enums/display_mode.py

from enum import Enum
from typing import Optional


class DisplayMode(Enum):
    """
    How the front-end should render this operation’s control, if at all.
    """
    OPERATION_NAVIGATION = "operation_navigation"  # show in main operation picker
    RESET_BUTTON         = "reset_button"          # render as a reset button
    UNDO_BUTTON          = "undo_button"           # render as an undo icon/button
    LOAD_BUTTON          = "load_button"           # render as a load-snapshot button
    SAVE_BUTTON          = "save_button"           # render as a save-snapshot button
    UPDATE_BUTTON        = "update_button"         # render as an update-snapshot button
    LOAD_OPTIONS         = "load_options"          # render as a dropdown of snapshot options
    NONE                 = None                    # no UI control displayed
