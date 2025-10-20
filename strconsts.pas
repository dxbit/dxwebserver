{-------------------------------------------------------------------------------

    Copyright 2016-2025 Pavel Duborkin ( mydataexpress@mail.ru )

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.

-------------------------------------------------------------------------------}

unit StrConsts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
  //rsOK = 'OK';
  //rsCancel = 'Cancel';
  //rsWarning = 'Warning';
  rsWeekNames = 'Monday Tuesday Wednesday Thursday Friday Saturday Sunday';
  rsWeekNamesBrief = 'Mon Tue Wed Thu Fri Sat Sun';
  rsMonthNames = 'January February March April May June July August September October November December';
  rsMonthNamesBrief = 'Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec';
  rsDateDetail = 'Day Week Month Quarter Half-Year Year';
  //rsCanNotDeleteRec = 'Can not delete record, because it use';
  //rsBack = 'Back';
  //rsExecute = 'Execute';
  //rsIncome = 'Income';
  //rsOutcome = 'Outcome';
  //rsTotal = 'Total';
  rsReports = 'Reports';
  rsData = 'Data';
  rsExit = 'Exit';
  //rsIncorrectPwd = 'Incorrect user or password';
  rsPassword = 'Password';
  rsLogin = 'Login';
  rsLoginTitle = 'Login';
  rsAppend = 'Append';
  //rsFilter = 'Filter';
  //rsDelete = 'Delete';
  //rsPrint = 'Print';
  //rsIncorrectFieldValue = 'Incorrect field value [%s]';
  //rsUpload = 'Upload';
  //rsUploadImage = 'Upload image';
  //rsUploadFile = 'Upload file';
  //rsHelp = 'Help';
  rsError = 'Error';
  //rsFileNotExists = 'File [%s] not exists';
  //rsEdit = 'Edit';
  //rsView = 'View';
  //rsIncompatibleTypes = 'Incompatible types';
  rsIllegalStrOp = 'Illegal string operation';
  rsIllegalNumOp = 'Illegal numeric operation';
  rsIllegalBoolOp = 'Illegal boolean operation';
  rsDivZero = 'Division by zero';
  rsInvalidNumber = 'Invalid number';
  rsArgExpected = 'Argument expected';
  rsEndLineExpected = 'End of line expected';
  rsSqBrExpected = 'Square bracket expected';
  //rsBrExpected = 'Bracket expected';
  //rsOpExpected = 'Operation expected';
  rsOperandExpected = 'Operand expected';
  rsFuncNotFound = 'Function not found';
  //rsIncorrectArgCount = 'Incorrect argument count';
  rsIncompatibleArg = 'Incompatible argument';
  rsCommaExpected = 'Comma expected';
  rsFieldNameEmpty = 'Field name is empty';
  //rsErrFieldNotFound = 'Field not found';
  //rsUnknownFieldSource = 'Unknown field source';
  rsNotObjField = 'Field is not object';
  rsIllegalDateOp = 'Illegal date/time operation';
  //rsGoto = 'Go&nbsp;To';
  rsNot = 'Not';
  //rsNull = 'Null';
  //rsAddVal = 'Add value';
  //rsTrue = 'True';
  //rsFalse = 'False';
  //rsFilterPresets = 'Filter presets';
  //rsSort = 'Sort';
  //rsDesc = 'Descending';
  rsRecordModified = 'Record modified';
  //rsRangeErrMsg = '%s: value should be from %s to %s';
  rsRequiredMsg = '%s: value is required';
  rsFPSrcFldNotFound = 'Source field [%s] not found';
  //rsFPSrcFldNotAccept = 'Source field [%s] unacceptable';
  rsFPSrcFldExcept = 'Source field expected';
  rsFPCmpOpExpect = 'Comparsion operation expected';
  //rsFPExprExpect = 'Expression expected';
  rsFPExprNotParsed = 'Expression not parsed';
  //rsFPErrInExpr = 'Error in expression ( %s ): %s';
  rsFPBoolOpExpect = 'Boolean operation expected';
  //rsFPSrcFldNotFoundIn = 'Source field [%s] not found in [%s]';
  //rsFPSrcFldNotAcceptPar = 'Source field [!%s] unacceptable';
  rsInvalidCmpOp = 'Invalid comparison operation';
  rsCharLBrExpext = 'Character { expected';
  rsCharRBrExpect = 'Character } expected';
  //rsVarNotFound = 'Variable %s not found';
  rsUser = 'User';
  //rsRemoteDB = 'Remote database';
  // Ваши права на просмотр этой формы/отчета были ограничены администратором базы данных.
  rsAccessDenied = 'Your rights to view this form/report have been restricted by the database administrator.';
  //rsUnsupported = 'Unsupported';
  //rsInvalidUseField = 'Invalid use of field';
  //rsDeleteLimitedMsg = 'Access to the table [%s] is limited. Deleting is not '
  //  +'possible.';
  rsFormNotAvail = 'In this expression forms is not available';
  rsFieldNotFound = 'Field not found: %s';
  rsFormQryNotFound = 'Form or query not found: %s';
  rsFieldNotDate = 'Field [%s] not date';
  rsFormNotFound = 'Form [%s] not found';
  rsReportNotFound = 'Report [%s] not found';
  rsText = 'Text';
  rsNumber = 'Number';
  rsDate = 'Date';
  rsBoolean = 'Boolean';
  //rsObject = 'Object';
  rsTime = 'Time';
  //rsCounter = 'Counter';
  //rsFile = 'File';
  rsUnknown = 'Unknown';
  rsIncompatibleTypesSrcFieldAndExpr = 'The result of the expression is not compatible with the source field. Got %s, expected %s.';
  rsObjFieldNotUseDBUnique = 'You cannot use the field of object [%s].';
  rsIncompatibleValueForField = 'The result of evaluating the expression is '
    +'incompatible with the field. For example, you can not write text '
    +'into a numeric field.';
  rsUnexpectedToken = 'Unexpected token %s';
  rsNumberOrText = 'Number or Text';
  rsInvalidOpForType = 'Operation %s is not valid for %s type.';
  rsInvalidOpForTypeExpected = 'Operation %s is not valid for %s type. Expected %s.';
  rsOpNotValidForDifOperands = 'Operation %s is not valid for operands of '
      +'different types: %s and %s. Operands must be of the same type.';
  rsExpectedClosingBracketOrOp = 'Expected closing bracket or operation.';
  rsFuncNotFound2 = 'Function not found: %s';
  rsInvalidNumberArgsFunc = 'Invalid number arguments of function %s. Got %d expected %d.';
  rsIncompatibleArgOfFunc = 'Incompatible argument #%d of function %s. Got %s '
      +'expected %s.';
  rsObjectNotHaveLinkForm = 'The object [%s] does not have an linked form.';
  rsFieldNotObject = 'Field [%s] not is objects.';
  rsObjectFieldNotFound = 'Object field not found: %s';
  rsSelCond = 'Selection condition';
  rsInvalidUsingGroupTag = 'Using more than one tag "group" in the same '
    +'paragraph or the same row of the table is invalid.';
  rsInvalidUseTagForm = 'Tags {form|%s} and {form|%s} are in the same paragraph. '
    +'Using more than one tag "form" in the same paragraph is invalid. ';
  rsInvalidUseTagGrid = 'Tags {grid|%s} and {grid|%s} are in the same table row. '
    +'Using more than one tag "grid" in the same table row is invalid.';
  rsTagWithoutClosingBracket = 'Tag {%s without a closing bracket }.';
  rsEndWthBegin = 'End tag without begin tag.';
  rsUnknownTag = 'Unknown tag: %s';
  rsFieldWithoutClosingBracket = 'Field [%s without a closing bracket ].';
  rsFmtDateMonth = 'January February March April May June July August September October November December';
  rsDBSumFieldNotNumeric = 'Field [%s] is incompatible with the function. The function works only with fields of the "Number" type.';
  rsQueryNotFound = 'Query not found: %s';
    rsIIFNullDetect = ' A null is detected. Verify that all operands of the logical expression have a specific values (not empty). To ensure a certain value, use function NZ or an additional null check. See also function NULL.';
  rsIIFParamNotLogic = 'The type of the result of evaluating a logical '
    +'expression is not a logical type (true or false).';
  rsCanNotChangeRec = 'Can not change the record. The record has already been saved or the changes have been canceled. Possible reasons: You are editing the records of one table at the same time in several tabs. You can only edit one table record at a time. Refresh your browser page to edit the record again.';
  rsCouldNotCreateFolder = 'Could not create folder: %s';
  rsCantCopyFileToStorageFolder = 'Can not copy file from %s to %s.';
  rsFormNotSel = 'Form not selected';
  rsMore = 'More';
  rsYes = 'Yes';
  rsNo = 'No';
  rsReportWindow = 'Report window';
  rsPrintErrMsg = 'An error occurred during printing: %s';
  rsDeleteRecMsg = 'The record will be deleted permanently. Continue?';
  rsConnecting = 'Connecting...';
  rsServerError = 'Server error';
  rsMenu = 'Menu';
  rsEmpty = 'Empty';
  rsWelcome = 'Welcome!';
  rsSelectAnyForm = 'Select any form.';
  rsEditRecNotFinished = 'You have not finished editing this record yet. You were automatically redirected to the edited record.';
  rsSubFormRecNotFound = 'No subform record was found, and you were redirected'
    +' to a main form record.';
  //rsFirstSaveRecord = 'First, save your new record.';
  //rsFormNotEditMode = 'The form is not in edit mode.';
  rsCouldNotOpenFormDataSet = 'Could not open form dataset [%s].';
  rsCouldNotOpenQueryDataSet = 'Could not open query dataset [%s].';
  rsDuplicateRecordWasCreated = 'A duplicate record was created.';
  rsPivotGridNotFound = 'Pivot grid not found: %s';
  rsServerItself = 'Server itself';
  //rsWelcomeToDXWebServer = 'Welcome to DataExpress Web Server!';
  //rsVersion = 'Version: %s';
  rsNoConnectionName = 'The connection name was not specified.';
  rsInvalidConnectionName = '%s - invalid connection name. The name can only '
    +'include the following characters: a-z, 0-9, _';
  rsConnectToDB = 'Connect to database';
  rsConnectionInProgress = 'A connection to the database is in progress. '
    +'Please wait...';
  rsTemplateNotFound = 'Template [%s] not found.';
  rsNoFormInDB = 'There are no forms in the database.';
  rsActionExecError = 'An error occurred while executing action [%s].%s';
  rsGoToForm = 'Go to form';
  rsPrint = 'Print';
  rsMassCalc = 'Mass calculation';
  rsOpenReport = 'Open report';
  rsSaveChanges = 'Save changes';
  //rsUserMonitor = 'User monitor';
  rsCallFunction = 'Call function';
  rsClearFields = 'Clear fields';
  rsShowMessage = 'Show message';
  //<p>Нет связи с формой. Действие не может быть завершено.</p><p>Возможная причина: Вы открыли одну и ту же запись в нескольких вкладках браузера одновременно и сохранили/отменили изменения в одной из вкладок. Обновите страницу браузера, чтобы снова связать форму.</p>
  rsNoLinkForm = 'The record cannot be edited. The action could not be completed. Possible cause: You opened the same record in several browser tabs at the same time and saved/discarded changes in one of the tabs. Refresh your browser page to continue editing the record again';
  rsNoLinkFormMsg = '<p>The record cannot be edited. The action could not be completed.</p><p>Possible cause: You opened the same record in several browser tabs at the same time and saved/discarded changes in one of the tabs. Go back to the previous page to continue editing the record again.</p>';
  rsDeleteRecLimited = 'Unable to delete a record because access to subordinate data is restricted.';
  rsRecordUsed = 'Cannot delete the record because it is in use.';
  // <p>Возможные причины: Вы редактируете записи одной таблицы одновременно в нескольких вкладках.</p><p>Вы можете редактировать только одну запись таблицы за раз.</p>
  rsRecordNotActive = '<p>The action could not be completed.</p><p>Possible reasons: You are editing the records of one table at the same time in several tabs. You can only edit one table record at a time.</p>';
  rsAnyFormNotFound = 'Form not found';
  rsAnyFormNotFoundMsg = 'It looks like you made a typo in the address, or this form no longer exists.';
  rsFilterError = 'Filter error';
  rsCantShowFilterMsg = 'Can not show filter, form not found. It looks like you made a typo in the address, or this form no longer exists.';
  rsCantApplyFilterMsg = 'Can not apply filter, form not found. It looks like you made a typo in the address, or this form no longer exists.';
  rsAnyRpNotFound = 'Report not found';
  rsAnyRpNotFoundMsg = 'It looks like you made a typo in the address, or this report no longer exists.';
  rsUnknownRequest = 'Unknown request';
  rsUnknownRequestMsg = 'The server cannot process your request. Make sure the URL is entered correctly.';
  rsUserMonitor = 'User monitor';
  rsNoNameUser = 'Noname user %d';
  rsUserName = 'Username';
  rsIP = 'IP';
  rsConnectTime = 'Connect time';
  rsElapsedTime = 'Elapsed time';
  rsElapsedTimeStr = '%d h %d m';
  rsType = 'Type';
  rsWeb = 'Web';
  rsDesktop = 'Desktop';
  rsAboutText = 'DataExpress Web Server<br>is working...<br><br>Version: %s<br><br>Copyright &#169; 2016-2025 Pavel Duborkin<br><br><a href="https://mydataexpress.ru">Project Website</a> | <a href="https://forum.mydataexpress.ru">Forum</a> | <a href="https://wiki.mydataexpress.ru">Wiki</a>';
  rsAboutTextConsole = 'DataExpress Web Server is working...' + LineEnding + 'Version: %s' + LineEnding + 'Copyright (c) 2016-2025 Pavel Duborkin';
  rsLoopDetectedCalc = 'A loop was detected while modifying the field or label [%s].';
  rsRecordNotFound = 'Record not found or access denied';
  rsRecordNotFoundMsg = 'Possible reasons:<ol><li>The record has been deleted.</li><li>The new record has not been saved.</li><li>An error was made in the URL.</li><li>Access denied by record selection condition.</li></ol>';
  rsRpUnderDevelopment = 'Report under development - no data sources specified.';
  //rsRecordNotFoundMsg = '<p>Запись не найдена. Возможные причины:<ol><li>Запись была удалена.</li><li>Новая запись не была сохранена.</li><li>Допущена ошибка в УРЛ.</li></ol></p>';
  rsForm = 'Form';
  rsExtension = 'Extension';
  rsScriptErrorMsg = '%sException class: %sModule name: %sModule kind: %sProcedure name: %s';
  rsCompileError = 'Compile error';
  rsLoadDataErrorMsg = 'Could not load script code.%sError message: %sModule name: %sModule kind: %s';
  rsRuntimeError = 'Runtime error';
  rsWebForm = 'Web form';
  rsWebExpression = 'Web expression';
  rsExecActionFailedMsg = 'Execute action %s failed in the extension module %s. Try to find and '
    +'remove the module that causes the error.';
  rsExceptionMsg = '%sException class: %s';
  rsExtModuleFailedMsg = 'Call function %s failed in the extension module %s. Try to find and '
    +'remove the module that causes the error.';
  rsInvalidMethotCall = 'Invalid method call child form: %s';
  rsComponentWithFieldNameNotFound = 'Component with the specified field name '
    +'[%s] not found.';
  rsPrintAborted = 'Print aborted.';
  rsAnotherRecordEditing = 'Another record is editing';
  rsInvalidFreshValueMsg = 'It looks like the web page contains stale '
    +'data. For example this is possible if you are working with the same '
    +'record in multiple tabs. The page will be updated.';
  rsInvalidFreshValue = 'Invalid fresh value.';
  rsClearAllFilters = 'Clear all filters';
  rsOutput = 'Output';
  rsButtonNotFound = 'Button not found.';
  rsIncorrectUserOrPwd = 'Incorrect user or password';
  rsConnectionNotFound = 'Connection %s not found.';
  rsUnknownFBVersion = 'Unknown Firebird version specified.';
  rsEmptySquareBracketsDetected = 'Empty square brackets detected';
  rsExprEmpty = 'Expression is empty';
  rsFieldNotFileImage = 'Field is not a file or image: %s';
  rsFieldNotImage = 'Field is not a image: %s';
  rsFuncNotImageFiles = 'The function cannot be applied to files and images.';
  rsInvalidDate = 'Invalid date %s';
  //rsRecordModifiedAnotherUser = 'The record has been modified by another user.';
  //rsRecordHasBeenDeleted = 'The record has been deleted.';

//const
  // Ответ сервера на ajax-запрос
  //rsReportNotFound = 'Report not found.';
  //rsTemplateNotFound = 'Template not found.';
  //rsRecordNotFoundOrNotActive = 'Record not found or not active.';

implementation

end.

