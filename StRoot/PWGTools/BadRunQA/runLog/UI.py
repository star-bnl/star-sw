from prompt_toolkit.application import Application
from prompt_toolkit.application.current import get_app
from prompt_toolkit.key_binding import KeyBindings
from prompt_toolkit.key_binding.bindings.focus import focus_next, focus_previous
from prompt_toolkit.layout import HSplit, Layout, VSplit, Dimension, Window
from prompt_toolkit.styles import Style
from prompt_toolkit.widgets import Box, Button, Frame, Label, TextArea
from prompt_toolkit.layout.controls import BufferControl, FormattedTextControl
from prompt_toolkit.key_binding.bindings.page_navigation import scroll_one_line_up, scroll_one_line_down
from enum import Enum

import shiftLogByShift as sl

TEXT = Enum('TEXT', 'BRIEF HISTORY SUMMARY', start=0)
STATUS = Enum('STATUS', 'GOOD BAD NOTSELECTED', start=0)

RESULT = None
KEYS = None
IDSTATUS = [] 
CURRID = 0
TEXTTYPE = TEXT.BRIEF
MULTABLE = False # disable left/right arrow if not multable
HIGHLIGHT = None # should be list of True/False with CURRID as index. Color on text area changes if True
HISTORYHIGHLIGHT = None
SUMMARYHIGHLIGHT = None
INTROTEXT = "Control with (up, down, left, right), Pg Up, Pg Down and Enter keys. Shortcut: q is good-run, w is bad-run and tab is go back."
REASONS = None # Reason for each run to be here. Should be list of string


def on_change():
    GoBackButton.text = 'Go Back'
    ExitButton.text = 'Exit %d/%d' % (CURRID+1, len(KEYS))
    if CURRID < len(KEYS):
        text_reason.text = 'Current run = %s for reason %s' % (KEYS[CURRID], REASONS[CURRID])
        if SUMMARYHIGHLIGHT is None:
            summary_sentement_label.text = ''
        elif SUMMARYHIGHLIGHT[CURRID]:
            summary_sentement_label.text = 'Bad Summary'
        else:
            summary_sentement_label.text = 'Good Summary'

        if HISTORYHIGHLIGHT is None:
            history_sentement_label.text = ''
        elif HISTORYHIGHLIGHT[CURRID]:
            history_sentement_label.text = 'Bad history'
        else:
            history_sentement_label.text = 'Good history'

        if HIGHLIGHT is None:
            entry_sentement_label.text = ''
        elif HIGHLIGHT[CURRID]:
            entry_sentement_label.text = 'Bad Entry'
        else:
            entry_sentement_label.text = 'Good Entry'

        if IDSTATUS[CURRID] == STATUS.GOOD:
            GoodRunButton.text = 'Good-run*'
        else:
            GoodRunButton.text = 'Good-run'
        if IDSTATUS[CURRID] == STATUS.BAD:
            BadRunButton.text = 'Bad-run*'
        else:
            BadRunButton.text = 'Bad-run'
        text_area.text = RESULT[KEYS[CURRID]][TEXTTYPE.value]
    else:
        get_app().exit()


# Event handlers for all the buttons.
def good_clicked():
    global RESULT, IDSTATUS, CURRID, KEYS, MULTABLE
    if GoodRunButton.text == '':
        return #button disabled if text on button is removed
    if CURRID >= 0:
        IDSTATUS[CURRID] = STATUS.GOOD
    else:
        MULTABLE = True
        left_clicked()
    CURRID = CURRID + 1
    MULTABLE = True
    on_change()

def bad_clicked():
    global RESULT, IDSTATUS, CURRID, KEYS, MULTABLE
    if BadRunButton.text == '':
        return 
    if CURRID >= 0:
        IDSTATUS[CURRID] = STATUS.BAD
    CURRID = CURRID + 1
    MULTABLE = True
    on_change()


def back_clicked():
    global RESULT, CURRID, MULTABLE
    if ExitButton.text == 'Confirm Exit':
        # abort exit. Go back to previous run
        MULTABLE = True
        on_change()
        return
    if CURRID < 0:
        return
    if CURRID > 0:
        CURRID = CURRID - 1
        MULTABLE = True
        on_change()


def exit_clicked():
    global RESULT, IDSTATUS, CURRID, KEYS, MULTABLE
    if CURRID < 0:
        return
    if ExitButton.text[:4] == 'Exit': # exit button is only pressed once
        ExitButton.text = 'Confirm Exit'
        GoodRunButton.text = ''
        BadRunButton.text = ''
        text_area.text = 'If exit, all runs beyond %s will be considered good runs.' % KEYS[CURRID]
        MULTABLE = False
    elif ExitButton.text == 'Confirm Exit':
        for i in range(CURRID, len(KEYS)):
            IDSTATUS[i] = STATUS.GOOD
        get_app().exit()

def left_clicked():
    global TEXTTYPE
    if MULTABLE and TEXTTYPE != TEXT.BRIEF:
        TEXTTYPE = TEXT(TEXTTYPE.value - 1)
        text_area.text = RESULT[KEYS[CURRID]][TEXTTYPE.value]
        text_content_type.text = "Content type " + TEXTTYPE.name
        LeftArrow.text = '' if TEXTTYPE == TEXT.BRIEF else TEXT(TEXTTYPE.value - 1).name
        RightArrow.text = '' if TEXTTYPE == TEXT.SUMMARY else TEXT(TEXTTYPE.value + 1).name

def right_clicked():
    global TEXTTYPE
    if MULTABLE and TEXTTYPE != TEXT.SUMMARY:
        TEXTTYPE = TEXT(TEXTTYPE.value + 1)
        text_area.text = RESULT[KEYS[CURRID]][TEXTTYPE.value]
        text_content_type.text = "Content type " + TEXTTYPE.name
        LeftArrow.text = '' if TEXTTYPE == TEXT.BRIEF else TEXT(TEXTTYPE.value - 1).name
        RightArrow.text = '' if TEXTTYPE == TEXT.SUMMARY else TEXT(TEXTTYPE.value + 1).name


# All the widgets for the UI.
GoodRunButton = Button("Next", handler=good_clicked, width=25)
BadRunButton  = Button("", handler=bad_clicked, width=25)
GoBackButton  = Button("", handler=back_clicked, width=25)
ExitButton    = Button("", handler=exit_clicked,  width=25)
LeftArrow     = Button("", handler=left_clicked, width=10, left_symbol='<', right_symbol='')
RightArrow    = Button("", handler=right_clicked, width=10, left_symbol='', right_symbol='>')


text_area = TextArea(focusable=False, scrollbar=True)
text_area_box = Box(body=Frame(text_area), padding=0, style='class:right-pane')

text_label = TextArea(focusable=False, scrollbar=False, style='class:label', height=Dimension(max=1))
text_label.text = INTROTEXT

text_reason = TextArea(focusable=False, scrollbar=False, style='class:label', height=Dimension(max=1))
text_content_type = TextArea(focusable=False, scrollbar=False, height=Dimension(preferred=1))

entry_sentement_label = TextArea(focusable=False, width=25)
entry_sentement_label.text = 'Entry is good' + '-'*20
def get_entry_style() -> str:
    if HIGHLIGHT is None or CURRID < 0:
        return 'class:left-pane'
    elif CURRID >= 0 and CURRID < len(HIGHLIGHT):
        if HIGHLIGHT[CURRID]:
            return 'class:indicator-bad'
    return 'class:indicator-good'

history_sentement_label = TextArea(focusable=False, width=25)
history_sentement_label.text = 'History is good'
def get_history_style() -> str:
    if HISTORYHIGHLIGHT is None or CURRID < 0:
        return 'class:left-pane'
    elif CURRID >= 0 and CURRID < len(HISTORYHIGHLIGHT):
        if HISTORYHIGHLIGHT[CURRID]:
            return 'class:indicator-bad'
    return 'class:indicator-good'


summary_sentement_label = TextArea(focusable=False, width=25)
summary_sentement_label.text = 'Summary is good'
def get_summary_style() -> str:
    if SUMMARYHIGHLIGHT is None or CURRID < 0:
        return 'class:left-pane'
    elif CURRID >= 0 and CURRID < len(SUMMARYHIGHLIGHT):
        if SUMMARYHIGHLIGHT[CURRID]:
            return 'class:indicator-bad'
    return 'class:indicator-good'

# Combine all the widgets in a UI.
# The `Box` object ensures that padding will be inserted around the containing
# widget. It adapts automatically, unless an explicit `padding` amount is given.
root_container = Box(
    HSplit(
        [
            text_label,
            text_reason,
            VSplit(
                [
                    Box(
                        body=HSplit([GoodRunButton, BadRunButton, GoBackButton, ExitButton,
                                     Box(body=TextArea(focusable=False), style='class:left-pane'),
                                     Box(body=entry_sentement_label, style=get_entry_style, height=1),
                                     Box(body=history_sentement_label, style=get_history_style, height=1),
                                     Box(body=summary_sentement_label, style=get_summary_style, height=1)], padding=1, width=25),
                        padding=1,
                        style="class:left-pane",
                    ),
                    HSplit([Box(body=VSplit([LeftArrow, Box(body=text_content_type, style='class:right-pane', width=Dimension(preferred=55)), RightArrow]), style='class:right-pane', height=1, padding=0),
                            text_area_box], padding=0)
                ], height=Dimension(preferred=70)
            ),
        ], 
    ),
)

layout = Layout(container=root_container, focused_element=GoodRunButton)


# Key bindings.
# also bind wasd for laptop users with not arrow keys
kb = KeyBindings()
kb.add("down")(focus_next)
kb.add("up")(focus_previous)

@kb.add("left")
def _(event):
    left_clicked()

@kb.add("right")
def _(event):
    right_clicked()

@kb.add("pageup")
def _(event):
    w = event.app.layout.current_window
    event.app.layout.focus(text_area.window)
    scroll_one_line_up(event)
    event.app.layout.focus(w)

@kb.add("pagedown")
def _(event):
    w = event.app.layout.current_window
    event.app.layout.focus(text_area.window)
    scroll_one_line_down(event)
    event.app.layout.focus(w)

@kb.add("w")
def _(event):
    event.app.layout.focus(BadRunButton)
    bad_clicked()

@kb.add("q")
def _(event):
    event.app.layout.focus(GoodRunButton)
    good_clicked()

@kb.add('tab')
def _(event):
    event.app.layout.focus(GoBackButton)
    back_clicked()



# Styling.
style = Style(
    [
        ("left-pane", "bg:#888800 #000000"),
        ("right-pane", "bg:#aaaaaa #000000"),
        ("indicator-bad", "bg:#aa0000 #000000"),
        ("indicator-good", "bg:#00aa00 #000000"),
        ("button", "#000000"),
        ("button-arrow", "#000000"),
        ("button focused", "bg:#ff0000"),
        ("text-area focused", "bg:#ff0000"),
        ("label", "bg:#000000"),
    ]
)


# Build a main application object.
application = Application(layout=layout, key_bindings=kb, style=style, full_screen=True, mouse_support=True)


def main(result, reasons, badKeys=None, badHistory=None, badSummary=None, intro=''):
    global RESULT, IDSTATUS, CURRID, KEYS, TEXTTYPE, HIGHLIGHT, REASONS, SUMMARYHIGHLIGHT, HISTORYHIGHLIGHT
    # remove empty entry
    KEYS = []
    RESULT = {}
    REASONS = []
    GoodRunButton.text = "Next"
    BadRunButton.text = ""
    GoBackButton.text = ""
    ExitButton.text = "" 
    text_reason.text = ""
    text_content_type.text = "" 
    summary_sentement_label.text = ""
    history_sentement_label.text = ""
    entry_sentement_label.text = ""
    TEXTTYPE = TEXT.HISTORY # the prompt is initialized by an automatic left click, so starts at center for the automatic left-click to be registered

    # hash table is more efficient for lookup
    if badKeys is not None:
        badKeys = set(badKeys)
        HIGHLIGHT = []
    if badHistory is not None:
        badHistory = set(badHistory)
        HISTORYHIGHLIGHT = []
    if badSummary is not None:
        badSummary = set(badSummary)
        SUMMARYHIGHLIGHT = []

    for key, content in result.items():
        KEYS.append(key)
        if badKeys is not None:
            if key in badKeys:
                HIGHLIGHT.append(True)
            else:
               HIGHLIGHT.append(False)
        if badHistory is not None:
            if key in badHistory:
                HISTORYHIGHLIGHT.append(True)
            else:
                HISTORYHIGHLIGHT.append(False)
        if badSummary is not None:
            if key in badSummary:
                SUMMARYHIGHLIGHT.append(True)
            else:
                SUMMARYHIGHLIGHT.append(False)
        brief = ('\n' + '-' * 50 + '\n' + '-'*50 + '\n').join([entry for _, entry in content.message.items()])
        if content.summary is None:
            summary = detailed = brief
        else:
            detailed = sl.printDict(key, content)
            summary = content.summary
        RESULT[key] = [brief, detailed, summary]
        REASONS.append(reasons[key])
        IDSTATUS.append(STATUS.NOTSELECTED)

    CURRID = -1
    text_area.text = intro
    application.run()
    pos = {}
    neg = {}
    for status, key in zip(IDSTATUS, KEYS):
        if status == STATUS.GOOD:
            pos[key] = result[key]
        elif status == STATUS.BAD:
            neg[key] = result[key]
        else:
            raise RuntimeError('Selection incomplete. This should not have happened.')
    return pos, neg


if __name__ == "__main__":
    results = {1: ['brief1', 'Problematic1 ', 'sum1'], 2: ['brief2', 'Problematic2', 'sum2'], 3: ['brief3', 'Non-problematic3', 'sum3']}
    reasons = {1: 'bad1', 2: 'bad2', 3: 'bad3'}
    pos, neg = main(results, reasons)#, [1, 3])
    print(pos, neg)
